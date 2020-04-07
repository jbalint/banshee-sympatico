//! Utility for finding uncommitted changes in Git repositories

#![deny(missing_docs)]

use std::error::Error;
use std::path::PathBuf;
use std::process::Command;

use git2::{Repository, Status, StatusOptions};

/// Locate the Git repos on the system
fn locate_git_repos() -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let locate_output =
        Command::new("locate")
            .args(&["-r", "\\.git$"])
            .output()?
            //.expect("Failed to run `locate`")
            .stdout
        ;
    Ok(String::from_utf8(locate_output)?
        .split("\n")
        .map(|p| PathBuf::from(p))
        .collect())
}

/// `main()`
fn main() {
    locate_git_repos()
        .expect("Failed to locate git repos")
        .iter()
        .filter_map(|p| p.parent())
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
                x if !x.is_empty() => Some((repo, x)),
                _ => None
            }
        })
        .for_each(|(repo, modified_paths)| {
            println!("Changed files in repo {}:\n{}\n\n\n",
                     repo.path().display(), modified_paths.join("\n"))
        })
    ;
}
