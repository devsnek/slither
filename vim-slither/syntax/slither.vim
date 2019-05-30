" Vim syntax file
" Language:     slither
" Maintainer:   Gus Caplan
" URL:          https://github.com/devsnek/slither

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'slither'
endif

" Dollar sign is permitted anywhere in an identifier
if (v:version > 704 || v:version == 704 && has('patch1142')) && main_syntax == 'slither'
  syntax iskeyword @,48-57,_,192-255,$
else
  setlocal iskeyword+=$
endif

syntax sync fromstart
" TODO: Figure out what type of casing I need
" syntax case ignore
syntax case match

syntax match   slNoise          /[:,;]/
syntax match   slDot            /\./ skipwhite skipempty nextgroup=slObjectProp,slFuncCall,slPrototype,slTaggedTemplate
syntax match   slObjectProp     contained /\<\K\k*/
syntax match   slFuncCall       /\<\K\k*\ze\s*(/
syntax match   slParensError    /[)}\]]/

" Program Keywords
syntax keyword slStorageClass   const let skipwhite skipempty nextgroup=slDestructuringBlock,slDestructuringArray,slVariableDef
syntax match   slVariableDef    contained /\<\K\k*/ skipwhite skipempty
syntax keyword slOperatorKeyword typeof void new skipwhite skipempty nextgroup=@slExpression
syntax match   slOperator       "[-!|&+<>=%/*~^]" skipwhite skipempty nextgroup=@slExpression
syntax match   slOperator       /::/ skipwhite skipempty nextgroup=@slExpression
syntax keyword slBooleanTrue    true
syntax keyword slBooleanFalse   false

" Modules
syntax keyword slImport                       import skipwhite skipempty nextgroup=slModuleAsterisk,slModuleKeyword,slModuleGroup
syntax keyword slExport                       export skipwhite skipempty nextgroup=@slAll,slModuleGroup,slExportDefault,slModuleAsterisk,slModuleKeyword,
syntax match   slModuleKeyword      contained /\<\K\k*/ skipwhite skipempty nextgroup=slModuleAs,slFrom,slModuleComma
syntax keyword slExportDefault      contained default skipwhite skipempty nextgroup=@slExpression
syntax keyword slExportDefaultGroup contained default skipwhite skipempty nextgroup=slModuleAs,slFrom,slModuleComma
syntax match   slModuleAsterisk     contained /\*/ skipwhite skipempty nextgroup=slModuleKeyword,slModuleAs,slFrom
syntax keyword slModuleAs           contained as skipwhite skipempty nextgroup=slModuleKeyword,slExportDefaultGroup
syntax keyword slFrom               contained from skipwhite skipempty nextgroup=slString
syntax match   slModuleComma        contained /,/ skipwhite skipempty nextgroup=slModuleKeyword,slModuleAsterisk,slModuleGroup

" Strings, Templates, Numbers
syntax region  slString           start=+\z(["']\)+  skip=+\\\%(\z1\|$\)+  end=+\z1+ end=+$+  contains=slSpecial,@Spell extend
syntax region  slTemplateString   start=+`+  skip=+\\`+  end=+`+     contains=slTemplateExpression,slSpecial,@Spell extend
syntax match   slTaggedTemplate   /\<\K\k*\ze`/ nextgroup=slTemplateString
syntax match   slNumber           /\c\<\%(\d\+\%(e[+-]\=\d\+\)\=\|0b[01]\+\|0o\o\+\|0x\x\+\)\>/
syntax keyword slNumber           Infinity
syntax match   slFloat            /\c\<\%(\d\+\.\d\+\|\d\+\.\|\.\d\+\)\%(e[+-]\=\d\+\)\=\>/

" Regular Expressions
syntax match   slSpecial            contained "\v\\%(x\x\x|u%(\x{4}|\{\x{4,5}})|c\u|.)"
syntax region  slTemplateExpression contained matchgroup=slTemplateBraces start=+$(+ end=+)+ contains=@slExpression keepend

" Objects
syntax match   slObjectShorthandProp contained /\<\k*\ze\s*/ skipwhite skipempty nextgroup=slObjectSeparator
syntax match   slObjectKey         contained /\<\k*\ze\s*:/ contains=slFunctionKey skipwhite skipempty nextgroup=slObjectValue
syntax region  slObjectKeyString   contained start=+\z(["']\)+  skip=+\\\%(\z1\|$\)+  end=+\z1\|$+  contains=slSpecial,@Spell skipwhite skipempty nextgroup=slObjectValue
syntax region  slObjectKeyComputed contained matchgroup=slBrackets start=/\[/ end=/]/ contains=@slExpression skipwhite skipempty nextgroup=slObjectValue,slFuncArgs extend
syntax match   slObjectSeparator   contained /,/
syntax region  slObjectValue       contained matchgroup=slObjectColon start=/:/ end=/[,}]\@=/ contains=@slExpression extend
syntax match   slObjectFuncName    contained /\<\K\k*\ze\_s*(/ skipwhite skipempty nextgroup=slFuncArgs
syntax match   slFunctionKey       contained /\<\K\k*\ze\s*:\s*function\>/
syntax match   slObjectMethodType  contained /\<[gs]et\ze\s\+\K\k*/ skipwhite skipempty nextgroup=slObjectFuncName
syntax region  slObjectStringKey   contained start=+\z(["']\)+  skip=+\\\%(\z1\|$\)+  end=+\z1\|$+  contains=slSpecial,@Spell extend skipwhite skipempty nextgroup=slFuncArgs,slObjectValue

exe 'syntax keyword slNull      null             '.(exists('g:slither_conceal_null')      ? 'conceal cchar='.g:slither_conceal_null       : '')
exe 'syntax keyword slReturn    return contained '.(exists('g:slither_conceal_return')    ? 'conceal cchar='.g:slither_conceal_return     : '').' skipwhite nextgroup=@slExpression'
exe 'syntax keyword slNan       NaN              '.(exists('g:slither_conceal_NaN')       ? 'conceal cchar='.g:slither_conceal_NaN        : '')
exe 'syntax keyword slPrototype prototype        '.(exists('g:slither_conceal_prototype') ? 'conceal cchar='.g:slither_conceal_prototype  : '')
exe 'syntax keyword slThis      this             '.(exists('g:slither_conceal_this')      ? 'conceal cchar='.g:slither_conceal_this       : '')
exe 'syntax keyword slSuper     super  contained '.(exists('g:slither_conceal_super')     ? 'conceal cchar='.g:slither_conceal_super      : '')

" Statement Keywords
syntax match   slBlockLabel              /\<\K\k*\s*::\@!/    contains=slNoise skipwhite skipempty nextgroup=slBlock
syntax match   slBlockLabelKey contained /\<\K\k*\ze\s*\_[;]/
syntax keyword slStatement     contained with yield debugger
syntax keyword slStatement     contained break continue skipwhite skipempty nextgroup=slBlockLabelKey
syntax keyword slConditional            if              skipwhite skipempty nextgroup=slParenIfElse
syntax keyword slConditional            else            skipwhite skipempty nextgroup=slCommentIfElse,slIfElseBlock
syntax keyword slConditional            switch          skipwhite skipempty nextgroup=slParenSwitch
syntax keyword slRepeat                 while for       skipwhite skipempty nextgroup=slParenRepeat,slForAwait
syntax keyword slDo                     do              skipwhite skipempty nextgroup=slRepeatBlock
syntax region  slSwitchCase   contained matchgroup=slLabel start=/\<\%(case\|default\)\>/ end=/:\@=/ contains=@slExpression,slLabel skipwhite skipempty nextgroup=slSwitchColon keepend
syntax keyword slTry                    try             skipwhite skipempty nextgroup=slTryCatchBlock
syntax keyword slFinally      contained finally         skipwhite skipempty nextgroup=slFinallyBlock
syntax keyword slCatch        contained catch           skipwhite skipempty nextgroup=slParenCatch
syntax keyword slException              throw
syntax keyword slAsyncKeyword           async await
syntax match   slSwitchColon   contained /::\@!/        skipwhite skipempty nextgroup=slSwitchBlock

" Keywords
syntax keyword slGlobalObjects      Promise
syntax keyword slExceptions         Error
syntax keyword slBuiltins           decodeURI decodeURIComponent encodeURI encodeURIComponent eval isFinite isNaN parseFloat parseInt uneval

" Code blocks
syntax region  slBracket                      matchgroup=slBrackets            start=/\[/ end=/\]/ contains=@slExpression,slSpreadExpression extend fold
syntax region  slParen                        matchgroup=slParens              start=/(/  end=/)/  contains=@slExpression extend fold
syntax region  slParenDecorator     contained matchgroup=slParensDecorator     start=/(/  end=/)/  contains=@slAll extend fold
syntax region  slParenIfElse        contained matchgroup=slParensIfElse        start=/(/  end=/)/  contains=@slAll skipwhite skipempty nextgroup=slCommentIfElse,slIfElseBlock,slReturn extend fold
syntax region  slParenRepeat        contained matchgroup=slParensRepeat        start=/(/  end=/)/  contains=@slAll skipwhite skipempty nextgroup=slCommentRepeat,slRepeatBlock,slReturn extend fold
syntax region  slParenSwitch        contained matchgroup=slParensSwitch        start=/(/  end=/)/  contains=@slAll skipwhite skipempty nextgroup=slSwitchBlock extend fold
syntax region  slParenCatch         contained matchgroup=slParensCatch         start=/(/  end=/)/  skipwhite skipempty nextgroup=slTryCatchBlock extend fold
syntax region  slFuncArgs           contained matchgroup=slFuncParens          start=/(/  end=/)/  contains=slFuncArgCommas,slComment,slFuncArgExpression,slDestructuringBlock,slDestructuringArray,slRestExpression skipwhite skipempty nextgroup=slCommentFunction,slFuncBlock extend fold
syntax region  slClassBlock         contained matchgroup=slClassBraces         start=/{/  end=/}/  contains=slClassFuncName,slClassMethodType,slArrowFunction,slArrowFuncArgs,slComment,slDecorator,slClassProperty,slClassPropertyComputed,slClassStringKey,slAsyncKeyword,slNoise extend fold
syntax region  slFuncBlock          contained matchgroup=slFuncBraces          start=/{/  end=/}/  contains=@slAll,slBlock extend fold
syntax region  slIfElseBlock        contained matchgroup=slIfElseBraces        start=/{/  end=/}/  contains=@slAll,slBlock extend fold
syntax region  slTryCatchBlock      contained matchgroup=slTryCatchBraces      start=/{/  end=/}/  contains=@slAll,slBlock skipwhite skipempty nextgroup=slCatch,slFinally extend fold
syntax region  slFinallyBlock       contained matchgroup=slFinallyBraces       start=/{/  end=/}/  contains=@slAll,slBlock extend fold
syntax region  slSwitchBlock        contained matchgroup=slSwitchBraces        start=/{/  end=/}/  contains=@slAll,slBlock,slSwitchCase extend fold
syntax region  slRepeatBlock        contained matchgroup=slRepeatBraces        start=/{/  end=/}/  contains=@slAll,slBlock extend fold
syntax region  slDestructuringBlock contained matchgroup=slDestructuringBraces start=/{/  end=/}/  contains=slDestructuringProperty,slDestructuringAssignment,slDestructuringNoise,slDestructuringPropertyComputed,slSpreadExpression,slComment extend fold
syntax region  slDestructuringArray contained matchgroup=slDestructuringBraces start=/\[/ end=/\]/ contains=slDestructuringPropertyValue,slNoise,slDestructuringProperty,slSpreadExpression,slDestructuringBlock,slDestructuringArray,slComment extend fold
syntax region  slObject             contained matchgroup=slObjectBraces        start=/{/  end=/}/  contains=slObjectKey,slObjectKeyString,slObjectKeyComputed,slObjectShorthandProp,slObjectSeparator,slObjectFuncName,slObjectMethodType,slComment,slObjectStringKey,slSpreadExpression,slDecorator,slAsyncKeyword extend fold
syntax region  slBlock                        matchgroup=slBraces              start=/{/  end=/}/  contains=@slAll,slSpreadExpression extend fold
syntax region  slModuleGroup        contained matchgroup=slModuleBraces        start=/{/ end=/}/   contains=slModuleKeyword,slModuleComma,slModuleAs,slComment skipwhite skipempty nextgroup=slFrom fold
syntax region  slSpreadExpression   contained matchgroup=slSpreadOperator      start=/\.\.\./ end=/[,}\]]\@=/ contains=@slExpression
syntax region  slRestExpression     contained matchgroup=slRestOperator        start=/\.\.\./ end=/[,)]\@=/
syntax region  slTernaryIf                    matchgroup=slTernaryIfOperator   start=/?:\@!/  end=/\%(:\|}\@=\)/  contains=@slExpression extend skipwhite skipempty nextgroup=@slExpression
syntax match   slOperator       /?\.\ze\_D/

syntax match   slFuncName             contained /\<\K\k*/ skipwhite skipempty nextgroup=slFuncArgs
syntax region  slFuncArgExpression    contained matchgroup=slFuncArgOperator start=/=/ end=/[,)]\@=/ contains=@slExpression extend
syntax match   slFuncArgCommas        contained ','
syntax keyword slArguments            contained arguments
syntax keyword slForAwait             contained await skipwhite skipempty nextgroup=slParenRepeat

" Matches a single keyword argument with no parens
syntax match   slArrowFuncArgs  /\<\K\k*\ze\s*=>/ skipwhite contains=slFuncArgs skipwhite skipempty nextgroup=slArrowFunction extend
" Matches a series of arguments surrounded in parens
syntax match   slArrowFuncArgs  /([^()]*)\ze\s*=>/ contains=slFuncArgs skipempty skipwhite nextgroup=slArrowFunction extend

exe 'syntax match slGenerator /\<generator\>/    skipwhite skipempty nextgroup=slFuncName,slFuncArgs skipwhite '.(exists('g:slither_conceal_function') ? 'conceal cchar='.g:slither_conceal_function : '')
exe 'syntax match slFunction /\<function\>/      skipwhite skipempty nextgroup=slFuncName,slFuncArgs skipwhite '.(exists('g:slither_conceal_function') ? 'conceal cchar='.g:slither_conceal_function : '')
exe 'syntax match slArrowFunction /=>/           skipwhite skipempty nextgroup=slFuncBlock,slCommentFunction '.(exists('g:slither_conceal_arrow_function') ? 'conceal cchar='.g:slither_conceal_arrow_function : '')
exe 'syntax match slArrowFunction /()\ze\s*=>/   skipwhite skipempty nextgroup=slArrowFunction '.(exists('g:slither_conceal_noarg_arrow_function') ? 'conceal cchar='.g:slither_conceal_noarg_arrow_function : '')
exe 'syntax match slArrowFunction /_\ze\s*=>/    skipwhite skipempty nextgroup=slArrowFunction '.(exists('g:slither_conceal_underscore_arrow_function') ? 'conceal cchar='.g:slither_conceal_underscore_arrow_function : '')

" Classes
syntax keyword slClassKeyword           contained class
syntax keyword slExtendsKeyword         contained extends skipwhite skipempty nextgroup=@slExpression
syntax match   slClassNoise             contained /\./
syntax match   slClassFuncName          contained /\<\K\k*\ze\s*[(<]/ skipwhite skipempty nextgroup=slFuncArgs
syntax match   slClassMethodType        contained /\<\%([gs]et\|static\)\ze\s\+\K\k*/ skipwhite skipempty nextgroup=slAsyncKeyword,slClassFuncName,slClassProperty
syntax region  slClassDefinition                  start=/\<class\>/ end=/\(\<extends\>\s\+\)\@<!{\@=/ contains=slClassKeyword,slExtendsKeyword,slClassNoise,@slExpression skipwhite skipempty nextgroup=slCommentClass,slClassBlock
syntax match   slClassProperty          contained /\<\K\k*\ze\s*=/ skipwhite skipempty nextgroup=slClassValue
syntax region  slClassValue             contained start=/=/ end=/\_[;}]\@=/ contains=@slExpression
syntax region  slClassPropertyComputed  contained matchgroup=slBrackets start=/\[/ end=/]/ contains=@slExpression skipwhite skipempty nextgroup=slFuncArgs,slClassValue extend
syntax region  slClassStringKey         contained start=+\z(["']\)+  skip=+\\\%(\z1\|$\)+  end=+\z1\|$+  contains=slSpecial,@Spell extend skipwhite skipempty nextgroup=slFuncArgs

" Destructuring
syntax match   slDestructuringPropertyValue     contained /\k\+/
syntax match   slDestructuringProperty          contained /\k\+\ze\s*=/ skipwhite skipempty nextgroup=slDestructuringValue
syntax match   slDestructuringAssignment        contained /\k\+\ze\s*:/ skipwhite skipempty nextgroup=slDestructuringValueAssignment
syntax region  slDestructuringValue             contained start=/=/ end=/[,}\]]\@=/ contains=@slExpression extend
syntax region  slDestructuringValueAssignment   contained start=/:/ end=/[,}=]\@=/ contains=slDestructuringPropertyValue,slDestructuringBlock,slNoise,slDestructuringNoise skipwhite skipempty nextgroup=slDestructuringValue extend
syntax match   slDestructuringNoise             contained /[,[\]]/
syntax region  slDestructuringPropertyComputed  contained matchgroup=slDestructuringBraces start=/\[/ end=/]/ contains=@slExpression skipwhite skipempty nextgroup=slDestructuringValue,slDestructuringValueAssignment,slDestructuringNoise extend fold

" Comments
syntax keyword slCommentTodo    contained TODO FIXME XXX TBD
syntax region  slComment        start=+//+ end=/$/ contains=slCommentTodo,@Spell extend keepend
syntax region  slComment        start=+/\*+  end=+\*/+ contains=slCommentTodo,@Spell fold extend keepend
syntax region  slEnvComment     start=/\%^#!/ end=/$/ display

" Specialized Comments - These are special comment regexes that are used in
" odd places that maintain the proper nextgroup functionality. It sucks we
" can't make slComment a skippable type of group for nextgroup
syntax region  slCommentFunction    contained start=+//+ end=/$/    contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slFuncBlock extend keepend
syntax region  slCommentFunction    contained start=+/\*+ end=+\*/+ contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slFuncBlock fold extend keepend
syntax region  slCommentClass       contained start=+//+ end=/$/    contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slClassBlock extend keepend
syntax region  slCommentClass       contained start=+/\*+ end=+\*/+ contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slClassBlock fold extend keepend
syntax region  slCommentIfElse      contained start=+//+ end=/$/    contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slIfElseBlock extend keepend
syntax region  slCommentIfElse      contained start=+/\*+ end=+\*/+ contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slIfElseBlock fold extend keepend
syntax region  slCommentRepeat      contained start=+//+ end=/$/    contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slRepeatBlock extend keepend
syntax region  slCommentRepeat      contained start=+/\*+ end=+\*/+ contains=slCommentTodo,@Spell skipwhite skipempty nextgroup=slRepeatBlock fold extend keepend

" Decorators
syntax match   slDecorator                    /^\s*@/ nextgroup=slDecoratorFunction
syntax match   slDecoratorFunction  contained /\h[a-zA-Z0-9_.]*/ nextgroup=slParenDecorator

syntax cluster slExpression  contains=slBracket,slParen,slObject,slTernaryIf,slTaggedTemplate,slTemplateString,slString,slNumber,slFloat,slOperator,slOperatorKeyword,slBooleanTrue,slBooleanFalse,slNull,slFunction,slGenerator,slArrowFunction,slGlobalObjects,slExceptions,slFutureKeys,slFuncCall,slNan,slPrototype,slBuiltins,slNoise,slClassDefinition,slArrowFunction,slArrowFuncArgs,slParensError,slComment,slArguments,slThis,slSuper,slDo,slForAwait,slAsyncKeyword,slStatement,slDot
syntax cluster slAll         contains=@slExpression,slStorageClass,slConditional,slRepeat,slReturn,slException,slTry,slNoise,slBlockLabel

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_slither_syn_inits")
  if version < 508
    let did_slither_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink slComment              Comment
  HiLink slEnvComment           PreProc
  HiLink slParensIfElse         slParens
  HiLink slParensRepeat         slParens
  HiLink slParensSwitch         slParens
  HiLink slParensCatch          slParens
  HiLink slCommentTodo          Todo
  HiLink slString               String
  HiLink slObjectKeyString      String
  HiLink slTemplateString       String
  HiLink slObjectStringKey      String
  HiLink slClassStringKey       String
  HiLink slTaggedTemplate       StorageClass
  HiLink slTernaryIfOperator    Operator
  HiLink slCharacter            Character
  HiLink slPrototype            Special
  HiLink slConditional          Conditional
  HiLink slBranch               Conditional
  HiLink slLabel                Label
  HiLink slReturn               Statement
  HiLink slRepeat               Repeat
  HiLink slDo                   Repeat
  HiLink slStatement            Statement
  HiLink slException            Exception
  HiLink slTry                  Exception
  HiLink slFinally              Exception
  HiLink slCatch                Exception
  HiLink slAsyncKeyword         Keyword
  HiLink slForAwait             Keyword
  HiLink slArrowFunction        Type
  HiLink slFunction             Type
  HiLink slGenerator            Type
  HiLink slArrowFuncArgs        slFuncArgs
  HiLink slFuncName             Function
  HiLink slFuncCall             Function
  HiLink slClassFuncName        slFuncName
  HiLink slObjectFuncName       Function
  HiLink slArguments            Special
  HiLink slError                Error
  HiLink slParensError          Error
  HiLink slOperatorKeyword      slOperator
  HiLink slOperator             Operator
  HiLink slOf                   Operator
  HiLink slStorageClass         StorageClass
  HiLink slClassKeyword         Keyword
  HiLink slExtendsKeyword       Keyword
  HiLink slThis                 Special
  HiLink slSuper                Constant
  HiLink slNan                  Number
  HiLink slNull                 Type
  HiLink slNumber               Number
  HiLink slFloat                Float
  HiLink slBooleanTrue          Boolean
  HiLink slBooleanFalse         Boolean
  HiLink slObjectColon          slNoise
  HiLink slNoise                Noise
  HiLink slDot                  Noise
  HiLink slBrackets             Noise
  HiLink slParens               Noise
  HiLink slBraces               Noise
  HiLink slFuncBraces           Noise
  HiLink slFuncParens           Noise
  HiLink slClassBraces          Noise
  HiLink slClassNoise           Noise
  HiLink slIfElseBraces         Noise
  HiLink slTryCatchBraces       Noise
  HiLink slModuleBraces         Noise
  HiLink slObjectBraces         Noise
  HiLink slObjectSeparator      Noise
  HiLink slFinallyBraces        Noise
  HiLink slRepeatBraces         Noise
  HiLink slSwitchBraces         Noise
  HiLink slSpecial              Special
  HiLink slTemplateBraces       Noise
  HiLink slGlobalObjects        Constant
  HiLink slGlobalNodeObjects    Constant
  HiLink slExceptions           Constant
  HiLink slBuiltins             Constant
  HiLink slImport               Include
  HiLink slExport               Include
  HiLink slExportDefault        StorageClass
  HiLink slExportDefaultGroup   slExportDefault
  HiLink slModuleAs             Include
  HiLink slModuleComma          slNoise
  HiLink slModuleAsterisk       Noise
  HiLink slFrom                 Include
  HiLink slDecorator            Special
  HiLink slDecoratorFunction    Function
  HiLink slParensDecorator      slParens
  HiLink slFuncArgOperator      slFuncArgs
  HiLink slClassProperty        slObjectKey
  HiLink slObjectShorthandProp  slObjectKey
  HiLink slSpreadOperator       Operator
  HiLink slRestOperator         Operator
  HiLink slRestExpression       slFuncArgs
  HiLink slSwitchColon          Noise
  HiLink slClassMethodType      Type
  HiLink slObjectMethodType     Type
  HiLink slClassDefinition      slFuncName
  HiLink slBlockLabel           Identifier
  HiLink slBlockLabelKey        slBlockLabel

  HiLink slDestructuringBraces     Noise
  HiLink slDestructuringProperty   slFuncArgs
  HiLink slDestructuringAssignment slObjectKey
  HiLink slDestructuringNoise      Noise

  HiLink slCommentFunction      slComment
  HiLink slCommentClass         slComment
  HiLink slCommentIfElse        slComment
  HiLink slCommentRepeat        slComment

  delcommand HiLink
endif

let b:current_syntax = "slither"
if main_syntax == 'slither'
  unlet main_syntax
endif
