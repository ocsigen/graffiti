
{client{

  let on_phonegap () =
    (Js.Unsafe.eval_string "'device' in window") &&
    (Js.Unsafe.eval_string "'cordova' in window.device")

  let launch_on_phonegap func =
    if on_phonegap ()
    then func ()
    else ()

  let not_launch_on_phonegap func =
    if on_phonegap ()
    then ()
    else func ()

  let download_file uri =
    let transfer = Js.Unsafe.eval_string "new FileTransfer()" in
    (Js.Unsafe.coerce transfer)##download(uri, "Graffiti/graffiti.png",
                                          Js.null, Js.null, true)
}}
