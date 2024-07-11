let attrOrig =
	Unix.tcgetattr Unix.stdin

let setupStdin () =
    let _hideCursor = Format.printf "\x1b[?25l%!" in
    let _, sy = ANSITerminal.size () in
    ANSITerminal.scroll sy;
    ANSITerminal.set_cursor 1 1;

    (* Modify stdin so that keystrokes are processed immediately *)
    (* Also ensure key input is not echoed *)
	let attr =
		{ attrOrig with c_icanon = false
		; c_echo = false
		}
	in
    Unix.(tcsetattr stdin TCSANOW attr)

let restoreAttributes () =
    let _showCursor = Format.printf "\x1b[?25h%!" in
	Unix.(tcsetattr stdin TCSANOW attrOrig)

let handle_signal =
	Sys.Signal_handle ( fun _ ->
        restoreAttributes ();
		exit 0
	)

let setup () =
	setupStdin ();
	Sys.(set_signal sigint handle_signal); (* Catch ctrl-c *)
	Sys.(set_signal sigterm handle_signal) (* Catch SIGTERM *)
