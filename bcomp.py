__author__ = 'igor'

from pyparsing import *


keywords = []


class ParserKeyword(Keyword):
    def __init__(self, matchString):
        super(ParserKeyword, self).__init__(matchString)
        keywords.append(matchString)


class Node(object):
    def __init__(self, loc):
        self.loc = loc

    def __len__(self):
        return 1

    def __getitem__(self, item):
        return self


class IdentifierNode(Node):
    def __init__(self, loc, identifier):
        super(IdentifierNode, self).__init__(loc)

        self.name = identifier

    def __str__(self):
        return self.name


class DeclarationNode(Node):
    def __init__(self, loc):
        super(DeclarationNode, self).__init__(loc)

        self.variables = []

    def __str__(self):
        return str(self.variables)


class FunctionCallNode(Node):
    def __init__(self, loc, name):
        super(FunctionCallNode, self).__init__(loc)

        self.name = name
        self.args = None

    def convert(self, value):
        self.args = []

        for exp in value:
            self.args.append(exp)

    def __str__(self):
        s = self.name + '('

        s += str(self.args[0])

        for arg in self.args[1:]:
            s += ', ' + str(arg)

        return s + ')'


class ExpressionNode(Node):
    def __init__(self, loc):
        super(ExpressionNode, self).__init__(loc)

        self.left = None
        self.right = None
        self.operator = None

    @property
    def single(self):
        return not self.operator and not self.right

    def convert(self, value):
        # FIXME: this won't work for unary operators
        if type(value) == ParseResults and len(value) != 3:
            value = value[0]

        size = len(value)

        if size == 1:
            self.left = value[0]
        elif size >= 3:
            self.left = ExpressionNode(self.loc)
            self.left.convert(value[0])

            self.operator = value[1]

            self.right = ExpressionNode(self.loc)

            if size == 3:
                self.right.convert(value[2])
            else:
                self.right.convert(value[2:])

    def __str__(self):
        if self.single:
            return str(self.left)
        return "%s %s %s" % (self.left, self.operator, str(self.right))


class ExternNode(Node):
    def __init__(self, loc, identifier):
        super(ExternNode, self).__init__(loc)

        self.identifier = identifier

    def __str__(self):
        return self.identifier


class IfNode(Node):
    def __init__(self, loc):
        super(IfNode, self).__init__(loc)

        self.expression = None
        self.block = None
        self.else_block = None


class WhileNode(Node):
    def __init__(self, loc):
        super(WhileNode, self).__init__(loc)

        self.expression = None
        self.block = None


class Function(object):
    def __init__(self, name, params):
        self.name = name
        self.params = params
        self.statements = []


class Parser(object):
    def expression(self, loc, value):
        node = ExpressionNode(loc)
        node.convert(value)

        return node

    def funcall(self, loc, value):
        node = FunctionCallNode(loc, value.func)
        node.convert(value.args)

        return node

    def auto(self, loc, value):
        if len(value) == 2:
            return value[0], value[1]
        else:
            return value[0], None

    def declaration(self, loc, value):
        node = DeclarationNode(loc)
        node.variables = list(value)

        return node

    def func(self, loc, value):
        f = Function(value.name, list(value.params))
        f.statements = list(value[2:])

        return f

    def extern(self, loc, value):
        return ExternNode(loc, value[0])

    def if_(self, loc, value):
        node = IfNode(loc)
        node.expression = value.expr
        node.block = value.block
        node.else_block = list(value.else_block)

        return node

    def while_(self, loc, value):
        node = WhileNode(loc)
        node.expression = value.expr
        node.block = value.block

        return node

    def global_declaration(self, loc, value):
        node = DeclarationNode(loc)
        node.variables = [(value.id, value.value)]

        return node

    def identifier(self, loc, value):
        # TODO: sym table checking, etc
        if value.name in keywords or not value.name:
            return value
        else:
            return IdentifierNode(loc, value.name)

    def __init__(self):
        identifier = Word(alphas + "_", alphanums + "_")('name').setParseAction(self.identifier)
        integer = Word(nums).setParseAction(int)
        char = Literal("'").suppress() + Word(alphanums, exact=1) + Literal("'").suppress()
        constant = (char | integer)
        plusop = oneOf('+ -')
        multop = oneOf('/ * %')
        logical_binary_operators = oneOf('& | ^')
        inc_op = Word('+', exact=2) | Word('-', exact=2)

        extern_statement = (ParserKeyword('extrn').suppress() + identifier + Suppress(';')).setParseAction(self.extern)

        auto_atom = (identifier + Optional(constant)).setParseAction(self.auto)
        declaration_statement = (
            ParserKeyword('auto').suppress() + auto_atom + ZeroOrMore(Literal(',').suppress() + auto_atom)
            + Suppress(';')
        ).setParseAction(self.declaration)

        precendence = [
            (multop, 2, opAssoc.LEFT),
            (plusop, 2, opAssoc.LEFT),
            (logical_binary_operators, 2, opAssoc.LEFT)
        ]

        expression = Forward()
        statement = Forward()
        block = Forward()

        arguments = delimitedList(expression)
        function_call = (identifier('func') + Suppress('(') + arguments('args') + Suppress(')'))\
            .setParseAction(self.funcall)

        assignment = identifier + Word('=') + expression

        expression << operatorPrecedence(function_call | assignment | identifier | constant, precendence)('expr')\
            .setParseAction(self.expression)

        expression_statement = expression + Suppress(';')

        if_statement = (
            ParserKeyword('if') + Suppress('(') + expression('expr') + Suppress(')') + block('block')
            + Optional(ParserKeyword('else').suppress() + block())('else_block')
        ).setParseAction(self.if_)

        while_statement = (
            ParserKeyword('while') + Suppress('(') + expression('expr') + Suppress(')') + block('block')
        ).setParseAction(self.while_)

        empty_statement = Suppress(';')

        # TODO: labels
        # TODO: switch/case

        statement << (
            expression_statement |
            declaration_statement |
            extern_statement |
            if_statement |
            while_statement |
            empty_statement
        )

        statement_list = ZeroOrMore(statement)
        block << (Suppress('{') + statement_list + Suppress('}')).setParseAction(list)

        parameter_list = delimitedList(identifier).setParseAction(list)

        function = (
            identifier('name') + Group(Suppress('(') + Optional(parameter_list) + Suppress(')'))('params') + block
        ).setParseAction(self.func)

        global_declaration = (identifier('id') + constant('value') + Suppress(';'))\
            .setParseAction(self.global_declaration)

        self.program = ZeroOrMore(global_declaration | function)

    def parse(self, code):
        return list(self.program.ignore(cStyleComment).parseString(code))


if __name__ == '__main__':
    p = Parser()

    code = p.parse("""
        printn(n,b) {
            extrn putchar;
            auto a;

            if(a=n/b) { /* assignment, not test for equality */
                printn(a, b); /* recursive */
            }
            else {
                while (a) {
                    a = c;
                }
            }
            putchar(n%b + '0');
        }

        a 12;
    """)

    print code