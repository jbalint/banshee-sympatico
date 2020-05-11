package jbalint.synthesis;

import java.util.UUID;

import com.intellij.compiler.server.BuildManagerListener;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

public class MyBuildManagerListener implements BuildManagerListener {

	@Override
	public void beforeBuildProcessStarted(@NotNull Project project, @NotNull UUID sessionId) {

	}

	@Override
	public void buildStarted(@NotNull Project project, @NotNull UUID sessionId, boolean isAutomake) {
		System.err.println("Build started for " + project);
	}

	@Override
	public void buildFinished(@NotNull Project project, @NotNull UUID sessionId, boolean isAutomake) {
		System.err.println("Build finished for " + project);

	}
}
