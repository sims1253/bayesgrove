Output tools/demo/repl-workflow/repl-remediation.mp4

Require R

Set Shell "bash"
Set FontSize 18
Set Width 1200
Set Height 800
Set Padding 25
Set Framerate 15
Set PlaybackSpeed 1
Set Theme "GruvboxDark"
Set WindowBar Colorful
Set TypingSpeed 50ms

Type "R --quiet"
Sleep 1s
Enter
Sleep 1s

Type "options(bg_demo_autostart = FALSE)"
Sleep 1s
Enter
Sleep 500ms

Type 'source("tools/demo/repl-workflow/launch-demo.R")'
Sleep 1s
Enter
Sleep 1s

Type 'demo_repl_workflow(checkpoint = "warning_branch", start_repl = TRUE)'
Sleep 1s
Enter
Sleep 3s

Type "# 1. Confirm the warning-driven workflow state"
Enter
Sleep 500ms
Type "status"
Sleep 1s
Enter
Sleep 2s

Type "# 2. Inspect the problematic fit's diagnostics"
Enter
Sleep 500ms
Type 'result "Fit Centered Parametrization"'
Sleep 1s
Enter
Sleep 3s

Type "# 3. Inspect the remediation action"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 3s

Type "# 4. Execute the template-backed remediation step"
Enter
Sleep 500ms
Type "do 2"
Sleep 1s
Enter
Sleep 1s

Type "Fit Non-Centered Revision"
Sleep 500ms
Enter
Sleep 500ms

Type "Y"
Sleep 500ms
Enter
Sleep 2s

Type "# 5. Run the revised branch"
Enter
Sleep 500ms
Type "run"
Sleep 1s
Enter
Sleep 4s

Type "# 6. Retire the stale warning branch"
Enter
Sleep 500ms
Type 'retire "Fit Centered Parametrization"'
Sleep 1s
Enter
Sleep 2s

Type "# 7. The project is now ready for comparison"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "exit"
Sleep 1s
Enter
Sleep 1s

Type 'q("no")'
Sleep 1s
Enter
