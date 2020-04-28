//! Utility for finding uncommitted changes in Git repositories

#![deny(missing_docs)]

use std::error::Error;
use std::path::{PathBuf, Path};
use std::process::Command;

use clap::{App, SubCommand};
use git2::{Repository, Status, StatusOptions};
use itertools::Itertools;
use reqwest::blocking::multipart::Form;
use reqwest::blocking::multipart::Part;
use reqwest::blocking::Client;
use serde::Serialize;

/// Git repository with a list of modified files
#[derive(Serialize, Debug)]
struct GitRepoWithModifications {
    repo_path: String,
    modified_files: Vec<String>,
}

/// Locate the Git repos on the system
fn locate_git_repos() -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let locate_output =
        Command::new("locate")
            .args(&["-r", "\\.git$"])
            .output()?
            .stdout
        ;
    Ok(String::from_utf8(locate_output)?
        .split("\n")
        .map(|p| PathBuf::from(p))
        .collect())
}

/// Given a list Git repos, return the ones which have modifications, including the modifications
fn find_repo_modifications(repo_locations: &Vec<PathBuf>) -> Vec<GitRepoWithModifications> {
    repo_locations
        .iter()
        .map(|p| Repository::open(p))
        .filter_map(Result::ok)
        .filter_map(|repo| {
            let changed_paths = repo.statuses(Some(StatusOptions::new().include_untracked(false)))
                .expect("getting statuses")
                .iter()
                .filter(|se| se.status() != Status::CURRENT)
                .filter_map(|se| se.path().map(String::from))
                .collect::<Vec<_>>()
                ;
            match changed_paths {
                x if !x.is_empty() =>
                    Some(GitRepoWithModifications {
                        repo_path: String::from(repo.path().to_str().unwrap()),
                        modified_files: x,
                    }),
                _ => None,
            }
        })
        .collect()
}

/// TODO : unused
/// Save the modified repos list along with their modifications
fn save_modified_repos(_repos_with_mods: &Vec<GitRepoWithModifications>) -> Result<(), Box<dyn Error>> {
    let form = Form::new()
        .text("database", "wordnet")
        .part("mappings", Part::file(Path::new("git_modified_repo.sms"))?)
        .text("input_file_type", "JSON")
        // .part("input_file", Part::stream("x"))
        ;
    let client = Client::new();
    let _res = client.post("https://localhost/stardog/admin/virtual/import")
        .basic_auth("admin", Some("admin"))
        .multipart(form)
        .send();
    Ok(())
}

///
fn print_modified_repo_json(repos_with_mods: &Vec<GitRepoWithModifications>) {
    println!("{}", repos_with_mods.into_iter()
        .map(|repo_with_mod| serde_json::to_string(&repo_with_mod).unwrap())
        .join("\n"))
}

/// `main()`
fn main() {
    let app_m = App::new("git_repo_monitor")
        .about("Find Git repositories needing attention")
        .subcommand(SubCommand::with_name("print_json")
            .about("Print the list of repositories as JSON"))
        .subcommand(SubCommand::with_name("save_to_stardog")
            .about("Save the list of repositories to Stardog"))
        .get_matches();

    let repos_with_mods =
        find_repo_modifications(&locate_git_repos().expect("Failed to locate repos"));

    match app_m.subcommand_name() {
        Some("print_json") =>
            print_modified_repo_json(&repos_with_mods),
        Some("save_to_stardog") =>
        // save_modified_repos(&repos_with_mods).expect("Save failed");
            println!("save_to_stardog command not implemented"),
        _ =>
            println!("No command specified"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_modified_files() {
        let x: GitRepoWithModifications;
        let y: Repository;
        // TODO : write me
    }
}
