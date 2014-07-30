extern crate libc;
use std::c_str::CString;

#[link(name = "readline")]
extern {
    fn readline(prompt: *const libc::c_char) -> *const libc::c_char;
    fn add_history(entry: *const libc::c_char);
}

fn prompt_for_input(prompt: &str) -> Option<String> {
    let prompt_c_str = prompt.to_c_str();
    unsafe {
        // wait for enter/CTRL-C/CTRL-D
        let raw = readline(prompt_c_str.as_ptr());

        // parse into String and return
        let cs = CString::new(raw, true);
        if cs.is_null() {
            None
        } else {
            // add to shell history unless it's an empty string
            if cs.len() > 0 {
                add_history(raw);
            }

            // return Option<String>
            cs.as_str().map(|s| s.to_str())
        }
    }
}

pub fn start(prompt: &str, f: |String| -> Result<String, String>) {
    loop {
        match prompt_for_input(prompt) {
            Some(input) => {
                if input.len() > 0 {
                    let result = f(input);
                    println!("{}", result.unwrap_or_else(|e| e));
                }
            },
            None => return
        };
    };
}
