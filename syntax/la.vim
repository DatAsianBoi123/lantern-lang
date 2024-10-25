sy case match

sy keyword laKeyword val fun ret break cont native
sy keyword laConditional if else
sy keyword laRepeat loop while
sy keyword laBoolean true false
sy keyword laConstant null
sy keyword laStruct rec

sy match laIdentifier /\<\w\+\>/
sy match laFunction /\(rec\s\)\@<!\<\w\+\>(\@=/
sy match laDelimiter /[\(\)\{\}\[\],.:]/
sy match laOperator /[=+\-*/|~<>!%?]\|&&\|<=\|>=\|\~?\||?/
sy match laNumber /\<\d\+\>\(\.\<\d*\>\)\?/
sy match laType /\(: *\)\@<=\<\w\+\>/
sy match laComment /\/\/[^\_]*/

sy region laString start=/"/ end=/"/
sy region laString start=/'/ end=/'/

hi link laKeyword Keyword
hi link laConditional Conditional
hi link laConstant Constant
hi link laOperator Operator
hi link laBoolean Boolean
hi link laString String
hi link laDelimiter Delimiter
hi link laIdentifier Identifier
hi link laFunction Function
hi link laNumber Number
hi link laType Type
hi link laRepeat Repeat
hi link laStruct Structure
hi link laComment Comment

