#!/usr/bin/env python
from pyparsing import ParseBaseException

__author__ = 'igor'

from pyparsing import *


keywords = []


class SymbolTable(object):
    def __init__(self, global_symbols):
        self.global_scope = Scope()
        self.global_scope.items = global_symbols
        self.current_scope = self.global_scope

    def add_function(self, name):
        self.global_scope.items.append(name)
        self._add(Scope(name))

    def begin_scope(self, name=''):
        if name:
            self.current_scope = next((scope for scope in self.global_scope.children if scope.name == name), None)
            if not self.current_scope:
                raise ParseBaseException("Unknown scope %s" % name)
        else:
            self._add(Scope())

    def close_scope(self):
        self.current_scope = self.current_scope.parent

    def _add(self, scope):
        scope.parent = self.current_scope
        self.current_scope.children.append(scope)
        self.current_scope = scope

    def in_scope(self, symbol):
        return symbol in self.current_scope


class Code(object):
    def __init__(self):
        self.functions = []
        self.declarations = []

        self.current_block = None

    def validate(self, sym_table):
        for function in self.functions:
            function.validate(sym_table)


class Scope(object):
    def __init__(self, name=''):
        self.parent = None
        self.children = []
        self.items = []
        self.name = name

    def __contains__(self, item):
        if self.parent:
            return item in self.items or item in self.parent
        else:
            return item in self.items

    def __str__(self):
        return str(self.name)


class ParserKeyword(Keyword):
    """
    A little workaround so the identifier function can ignore keywords
    """
    def __init__(self, matchString):
        super(ParserKeyword, self).__init__(matchString)
        keywords.append(matchString)


class Node(object):
    def __init__(self, loc):
        self.loc = loc

    def validate(self, sym_table):
        """

        :param sym_table:SymbolTable
        :return:
        """
        pass

    def __add__(self, other):
        if type(other) is list:
            return [self] + other
        return self + other


class LiteralNode(Node):
    def __init__(self, loc, value, ttype):
        super(LiteralNode, self).__init__(loc)
        self.value = value
        self.type = ttype


class IdentifierNode(Node):
    def __init__(self, loc, identifier):
        super(IdentifierNode, self).__init__(loc)

        self.name = identifier

    def __str__(self):
        return self.name

    def __eq__(self, other):
        if type(other) is IdentifierNode:
            return other.name == self.name
        elif type(other) is str:
            return self.name == other

        return self == other

    def validate(self, sym_table):
        if self.name not in sym_table.current_scope:
            raise ParseBaseException('Symbol "%s" not declared in current scope' % self.name, self.loc)


class DeclarationNode(Node):
    def __init__(self, loc):
        super(DeclarationNode, self).__init__(loc)

        self.variables = []

    def __str__(self):
        return str(self.variables)

    def validate(self, sym_table):
        # TODO: maybe set the value as well?

        for var, val in self.variables:
            if var in sym_table.current_scope:
                raise ParseBaseException('Symbol "%s" already declared' % var, self.loc)
            sym_table.current_scope.items.append(var)


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

    def validate(self, sym_table):
        if not sym_table.in_scope(self.name):
            raise ParseBaseException("Function %s not found" % self.name, self.loc)


class ExpressionNode(Node):
    def __init__(self, loc):
        super(ExpressionNode, self).__init__(loc)

        self.left = None
        self.right = None
        self.operator = None

    @property
    def single(self):
        """
        :return: True if only a single number or variable is hold
        """
        return not self.operator and not self.right

    def convert(self, value):
        """
        Converts a ParseResults object into a valid ExpressionNode-based tree, which can be later used by the compiler

        :param value:
        """
        # FIXME: this won't work for unary operators
        if type(value) == ParseResults and len(value) != 3:
            value = value[0]

        size = len(value) if not isinstance(value, Node) else 1

        if size == 1:
            self.left = value[0] if not isinstance(value, Node) else value
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

    def validate(self, sym_table):
        self.left.validate(sym_table)
        if not self.single:
            self.right.validate(sym_table)


class ExternNode(Node):
    def __init__(self, loc, identifier):
        super(ExternNode, self).__init__(loc)

        self.identifier = identifier

    def __str__(self):
        return self.identifier

    def validate(self, sym_table):
        if self.identifier in sym_table.current_scope:
            raise ParseBaseException('Symbol "%s" already declared' % self.identifier, self.loc)
        sym_table.current_scope.items.append(self.identifier)


class IfNode(Node):
    def __init__(self, loc):
        super(IfNode, self).__init__(loc)

        self.expression = None
        self.block = None
        self.else_block = None

    def validate(self, sym_table):
        if not isinstance(self.block, list):
            self.block = [self.block]

        self.expression.validate(sym_table)

        sym_table.begin_scope()

        for statement in self.block:
            statement.validate(sym_table)

        sym_table.close_scope()
        sym_table.begin_scope()

        for statement in self.else_block:
            statement.validate(sym_table)

        sym_table.close_scope()


class WhileNode(Node):
    def __init__(self, loc):
        super(WhileNode, self).__init__(loc)

        self.expression = None
        self.block = None

    def validate(self, sym_table):
        if not isinstance(self.block, list):
            self.block = [self.block]

        if isinstance(self.block, Node):
            self.block = [self.block]

        sym_table.begin_scope()

        for statement in self.block:
            statement.validate(sym_table)

        sym_table.close_scope()


class Function(object):
    def __init__(self, name, params):
        self.name = name
        self.params = params
        self.statements = []

    def validate(self, sym_table):
        sym_table.begin_scope(self.name)

        for param in self.params:
            sym_table.current_scope.items.append(param)

        for statement in self.statements:
            statement.validate(sym_table)

        sym_table.close_scope()


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

        self.sym_table.add_function(f.name)
        self.code.functions.append(f)

        return f

    def extern(self, loc, value):
        node = ExternNode(loc, value[0])
        return node

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

        self.code.declarations.append(node)

        return node

    def identifier(self, loc, value):

        # TODO: sym table checking, etc
        if value.name in keywords or not value.name:
            return value
        else:
            return IdentifierNode(loc, str(value.name))

    def block(self, loc, value):
        return list(value)

    def integer(self, loc, value):
        return LiteralNode(loc, value, 'integer')

    def char(self, loc, value):
        return LiteralNode(loc, value, 'char')

    def __init__(self):
        self.sym_table = SymbolTable([])
        self.code = Code()

        identifier = Forward()
        integer = Word(nums).setParseAction(self.integer)
        char = (Literal("'").suppress() + Word(alphanums, exact=1) + Literal("'").suppress()).setParseAction(self.char)
        literal = (char | integer)
        plusop = oneOf('+ -')
        multop = oneOf('/ * %')
        logical_binary_operators = oneOf('& | ^')
        inc_op = Word('+', exact=2) | Word('-', exact=2)

        # TODO: strings

        external_statement = (ParserKeyword('extrn').suppress() + identifier + Suppress(';'))\
            .setParseAction(self.extern)

        auto_atom = (identifier + Optional(literal)).setParseAction(self.auto)
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

        expression << operatorPrecedence(function_call | assignment | identifier | literal, precendence)('expr')\
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
            external_statement |
            if_statement |
            while_statement |
            empty_statement
        )

        statement_list = ZeroOrMore(statement)
        block << (Suppress('{') + statement_list + Suppress('}')).setParseAction(self.block)

        parameter_list = delimitedList(identifier).setParseAction(list)

        function = (
            identifier('name') + Group(Suppress('(') + Optional(parameter_list) + Suppress(')'))('params') + block
        ).setParseAction(self.func)

        global_declaration = (identifier('id') + literal('value') + Suppress(';'))\
            .setParseAction(self.global_declaration)

        self.program = ZeroOrMore(global_declaration | function)

        identifier << (~MatchFirst(map(Keyword, keywords)) + Word(alphas + "_", alphanums + "_")('name'))\
            .setParseAction(self.identifier)

    def parse(self, source_code):
        return list(self.program.ignore(cStyleComment).parseString(source_code, parseAll=True))

    def validate(self):
        self.code.validate(self.sym_table)


if __name__ == '__main__':
    p = Parser()

    code = p.parse("""
        printn(n,b) {
            extrn putchar;
            auto a;

            if (a=n/b) { /* assignment, not test for equality */
                printn(a, b); /* recursive */
            }
            else {
                while (a) {
                    a = b;
                }
            }
            putchar(n%b + '0');
        }

    """)

    p.validate()

    print code
