Output tools/demo/repl-workflow/repl-comparison.mp4

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

Type 'demo_repl_workflow(checkpoint = "comparison_ready", start_repl = TRUE)'
Sleep 1s
Enter
Sleep 3s

Type "# 1. Confirm the project is comparison-ready"
Enter
Sleep 500ms
Type "status"
Sleep 1s
Enter
Sleep 2s

Type "# 2. Inspect the branch_comparison template action"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 3. Create the comparison node"
Enter
Sleep 500ms
Type "do 1"
Sleep 1s
Enter
Sleep 1s

Type "Compare Baseline vs Revision"
Sleep 500ms
Enter
Sleep 2s

Type "# 4. Run the comparison result"
Enter
Sleep 500ms
Type "run"
Sleep 1s
Enter
Sleep 4s

Type "# 5. Review the next decision step"
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
