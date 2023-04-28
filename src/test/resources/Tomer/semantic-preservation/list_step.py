#! Start of synth number: 1 of: letters
#! 1) s = 'abcdefg' => letters = ['a','c','e','g'] 
letters = list(s)[::2]
#! End of synth number: 1

#! Start of synth number: 2 of: rs
#! 1) s = 'abcdefg', letters = ['a', 'c', 'e', 'g'] => rs = "a.c.e.g" 
rs = ".".join(letters)
#! End of synth number: 2