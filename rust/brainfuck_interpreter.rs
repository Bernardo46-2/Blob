use std::io::{stdin, stdout, Write};

// s              +[--------->++<]>+.
// s              +++++++++++[>++++++++++<-]>+++++.
// John           +++++ ++ [ > +++++ +++++ < -] >++++. [-] +++++ +++++ + [ > +++++ +++++ < - ] > + . --- --- - . > [-] +++ [< ++ > -] < .
// Hello, World!  >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+.
// fibonacci      +++++++++++>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]
// factorial      >++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]

fn clear(){
    print!("\x1B[2J\x1B[1;1H");
}

fn to_ascii(i: &i32) -> String{
    match *i{
        x@0..=127 => format!("{}", x as u8 as char),
        _ => "".into(),
    }
}

fn read(input: &mut String){
    stdout().flush()
        .expect("failed to flush");
    stdin().read_line(input)
        .expect("failed to read");
}

fn read_u8() -> u8{
    let mut input = String::new();

    read(&mut input);
    let input: u8 = input.trim().parse().unwrap();

    input
}

fn read_string() -> String{
    let mut input = String::new();
    
    read(&mut input);

    input.trim().to_string()
}

fn find_closing_bracket(s: &str, open_bracket: &usize) -> usize{
    let mut x = 1;
    let mut closing_bracket: usize = *open_bracket;

    while x > 0 && closing_bracket < s.len(){
        closing_bracket += 1;
        
        if s.chars().nth(closing_bracket).unwrap() == '['{
            x += 1;
        }
        else if s.chars().nth(closing_bracket).unwrap() == ']'{
            x -= 1;
        }
    }

    closing_bracket
}

fn interpret_loop(code: &str, vec: &mut Vec<u8>, ptr: &mut usize){
    while vec[*ptr] != 0{
        let mut i = 0;

        while i < code.len(){
            interpret_char(code, vec, ptr, &mut i);
            i += 1;
        }
    }
}

fn interpret_char(code: &str, vec: &mut Vec<u8>, ptr: &mut usize, c: &mut usize){
    let current_char: char = code.chars().nth(*c).unwrap();

    match current_char{
        '>' => if *ptr < vec.len() - 1{
                *ptr += 1;
            },
        '<' => if *ptr > 0{
                *ptr -= 1;
            },
        '+' => {
            if vec[*ptr] < 255{
                vec[*ptr] += 1;
            }else{
                vec[*ptr] = 0;
            }
        },
        '-' => {
            if vec[*ptr] > 0{
                vec[*ptr] -= 1;
            }else{
                vec[*ptr] = 255;
            }
        },
        '.' => print!("{}", to_ascii(&(vec[*ptr] as i32))),
        ',' => vec[*ptr] = read_u8(),
        '[' => {
            let closing_bracket = find_closing_bracket(code, c);
            interpret_loop(&code[*c + 1..closing_bracket], vec, ptr);
            *c = closing_bracket;
        },
        _ => {}
    }
}

fn interpret_code(code: &str){
    let mut i = 0usize;
    let mut ptr = 0usize;
    let mut vec: Vec<u8> = vec![0; 30000];

    while i < code.len(){
        interpret_char(&code, &mut vec, &mut ptr, &mut i);
        i += 1;
    }
}

fn main(){
    clear();

    loop{
        let code = read_string();
        interpret_code(&code);
        println!("");

        if code == "exit"{break}
    }
}