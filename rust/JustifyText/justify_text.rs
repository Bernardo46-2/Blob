// Write an algorithm to justify text. Given a sequence of words and an integer line 
// length k, return a list of strings which represents each line, fully justified.

// More specifically, you should have as many words as possible in each line. There 
// should be at least one space between each word. Pad extra spaces when necessary
//  so that each line has exactly length k. Spaces should be distributed as equally 
// as possible, with the extra spaces, if any, distributed starting from the left.
// If you can only fit one word on a line, then you should pad the right-hand side with spaces.
// Each word is guaranteed not to be longer than k.

// For example, given the list of words ["the", "quick", "brown", "fox", "jumps", 
// "over", "the", "lazy", "dog"] and k = 16, you should return the following:

// ["the  quick brown", # 1 extra space on the left
//  "fox  jumps  over", # 2 extra spaces distributed evenly
//  "the   lazy   dog"] # 4 extra spaces distributed evenly


fn add_spaces(vec: &mut Vec<String>, i: usize, k: usize){
    let mut spaces_needed = k - vec[i].len();
    let mut spaces_found = 0;
    
    if spaces_needed > 0{
        while spaces_needed > 0 && vec[i].len() <= k{
            let mut j = 0;

            while j < vec[i].len(){
                if j == vec[i].len() - 1 && j + 1 < k && spaces_found == 0{
                    vec[i].push(' ');

                    if spaces_needed > 0{
                        spaces_needed -= 1;
                    }
                }else if vec[i].chars().nth(j).unwrap() == ' ' && vec[i].len() < k{
                    spaces_found += 1;
                    
                    vec[i].insert(j, ' ');

                    if spaces_needed > 0{
                        spaces_needed -= 1;
                    }

                    j += 2;
                }

                j += 1;
            }
        }
    }
}

fn remove_empty_str(vec: &mut Vec<String>){
    for i in (0..vec.len()).rev(){
        if vec[i].is_empty(){
            vec.remove(i);
        }
    }
}

fn justify(vec: Vec<&str>, k: usize) -> Vec<String>{
    let mut justified: Vec<String> = Vec::new();
    justified.push(String::new());
    
    for i in 0..vec.len(){
        let last = if justified.len() == 0 {0} else {justified.len() - 1};
        let push = justified[last].len() + vec[i].len() + 1 > k;

        if push{
            justified.push(String::new());
        }
        
        let mut tmp_str = String::from(vec[i]);
        
        if tmp_str.len() + justified[last].len() + 1 <= k{
            if tmp_str.len() < k && !justified[last].is_empty(){
                tmp_str.insert(0, ' ');
            }

            let last = justified.len() - 1;
            justified[last].push_str(&tmp_str);
            
        }else if tmp_str.len() <= k{
            justified.push(tmp_str);
        }
    }

    remove_empty_str(&mut justified);    

    for i in 0..justified.len(){
        add_spaces(&mut justified, i, k);
    }

    justified
}

pub fn main(){
    // let vec = vec!["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"];
    let vec = vec!["Lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipisicing", "elit", "Voluptatum", "quidem", "qui", "ipsa", "nihil", "quis", "modi", "aliquam", "minima", "hic", "suscipit!", "Iure"];
    let k = 24;

    let justified = justify(vec, k);
    for line in justified{
        println!("{:?}", line);
    }
}
