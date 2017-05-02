operators = {
    'at' :"@",
    'colon' :":",
    'comma' :",",
    'divide' :"/",
    'dot' :".",
    'equals' :"=",
    'larrow' :"<-",
    'lbrace' :"{",
    'le' :"<=",
    'lparen' :"(",
    'lt' :"<",
    'minus' :"-",
    'plus' :"+",
    'rarrow' :"=>",
    'rbrace' :"}",
    'rparen' :")",
    'semi' :";",
    'tilde' :"~",
    'times' :"*"
}

def get_symbol(token_type):
    if token_type in operators:
        return operators[token_type]
    else:
        return token_type