#! Start of synth number: 1 of: words
#! 1) a = 'Tomer katz' => words = ['Tomer','katz'] 
words = a.split()
#! End of synth number: 1
#! Start of synth number: 2 of: initials
#! 1) a = 'Tomer katz', words = ['Tomer', 'katz'] => initials = 'T.K' 
initials = ".".join([var[0] for var in words]).upper()
#! End of synth number: 2