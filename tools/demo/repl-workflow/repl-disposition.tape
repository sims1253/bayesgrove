Output tools/demo/repl-workflow/repl-disposition.mp4

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

Type 'demo_repl_workflow(checkpoint = "disposition_ready", start_repl = TRUE)'
Sleep 1s
Enter
Sleep 3s

Type "# 1. Inspect the remaining branch obligation"
Enter
Sleep 500ms
Type "status"
Sleep 1s
Enter
Sleep 2s

Type "# 2. The current branch scope only needs an explicit disposition"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 3. Accept the revised branch"
Enter
Sleep 500ms
Type "do 1"
Sleep 1s
Enter
Sleep 1s

Type "1"
Sleep 500ms
Enter
Sleep 500ms

Type "Promote the revised branch as the accepted analysis path."
Sleep 500ms
Enter
Sleep 2s

Type "# 4. The branch scope is now healthy"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "nodes"
Sleep 1s
Enter
Sleep 3s

Type "exit"
Sleep 1s
Enter
Sleep 1s

Type 'q("no")'
Sleep 1s
Enter
