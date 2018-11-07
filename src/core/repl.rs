extern crate libc;
use std::ffi::CString;
use std::ffi::CStr;

#[link(name = "readline")]
extern {
    fn readline(prompt: *const libc::c_char) -> *const libc::c_char;
    fn add_history(entry: *const libc::c_char);
}

fn prompt_for_input(prompt: &str) -> Option<String> {
    let prompt_c_str = CString::new(prompt).unwrap();

    unsafe {
        // wait for enter/CTRL-C/CTRL-D
        let raw = readline(prompt_c_str.as_ptr());
        if raw.is_null() {
            return None;
        }

        // parse into String and return
        let buf = CStr::from_ptr(raw).to_bytes();
        let cs = String::from_utf8(buf.to_vec()).unwrap();

        // add to shell history unless it's an empty string
        if cs.len() > 0 {
            add_history(raw);
        }

        // return Option<String>
        Some(cs)
    }
}

pub fn start<F: Fn(String) -> Result<String, String>>(prompt: &str, f: F) {
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
