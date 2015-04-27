# Copyright (C) 2015 Igor de Almeida
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

from pyparsing import *


class Node(object):
    def __init__(self, ttype="", value=None):
        self.child_nodes = []
        self.type = ttype
        self.has_block = False
        self.value = None

        self.format_value(value)

    def __str__(self):
        ret = "%s %s\n" % (self.type, str(self.value))

        ret += ''.join(map(str, self.child_nodes))

        return ret

    def format_value(self, value):
        self.value = value


class ExpressionNode(Node):
    pass


class ConditionalNode(Node):
    def __init__(self):
        super(ConditionalNode, self).__init__()

        self.conditional_expression = None
        self.has_block = True


class IfNode(ConditionalNode):
    def __init__(self):
        super(IfNode, self).__init__()

        self.else_nodes = None
        self.has_block = True
        self.type = 'if'

    def __str__(self):
        ret = "%s %s" % (self.type, str(self.conditional_expression))

        ret += ''.join(map(str, self.child_nodes))

        if len(self.else_nodes):
            ret += "else\n"
            ret += ''.join(map(str, self.else_nodes))

        return ret


class Function(object):
    # TODO: There should be reference to the sym table so it is possible to distinguish between local and global scope
    def __init__(self, name, params):
        self.name = name
        self.params = params
        self.root_node = Node()
        self.root_node.type = 'root'

    def __str__(self):
        return "%s %s\n%s" % (self.name, str(self.params), str(self.root_node))


class CodeTree(object):
    def __init__(self):
        self.global_vars = []
        self.functions = []

        self.current_node = None

    def add_function(self, function):
        self.functions.append(function)

    def __str__(self):
        ret = ''

        for function in self.functions:
            ret += str(function) + "\n"

        return ret


class SymbolicTable(object):
    pass


class Parser(object):
    def __init__(self, debug=False):
        self.code = CodeTree()
        self.sym = SymbolicTable()

        self.debug = debug

        identifier = Word(alphas + "_", alphanums + "_")
        integer = Word(nums).setParseAction(lambda x: int((x[0])))
        char = Literal("'").suppress() + Word(alphanums, exact=1) + Literal("'").suppress()
        constant = (char | integer)
        lpar = Literal('(').suppress()
        rpar = Literal(')').suppress()
        plusop = oneOf('+ -')
        multop = oneOf('/ * %')
        logical_binary_operators = oneOf('& | ^')
        inc_op = Word('+', exact=2) | Word('-', exact=2)

        expression = Forward()
        statement = Forward()
        statement_list = Forward()

        arguments = delimitedList(expression)
        function_call = (identifier('name') + lpar + Optional(arguments)('args') + rpar).setParseAction(
            self.function_call_action)

        precendence = [
            (multop, 2, opAssoc.LEFT),
            (plusop, 2, opAssoc.LEFT),
            (logical_binary_operators, 2, opAssoc.LEFT)
        ]

        inc_expr = ((identifier + inc_op) | (inc_op + identifier))

        expression << (operatorPrecedence(function_call | constant | identifier, precendence)).setParseAction(
            self.expression_action)

        assignment = Forward()
        assignment << (identifier + Word('=').suppress() + (expression | assignment)('value'))\
            .setParseAction(self.assignment_action)

        # statements
        return_statement = (Keyword('return').suppress() + expression + Suppress(';'))\
            .setParseAction(self.return_action)

        external_statement = (Keyword('extrn').suppress() + identifier('name') + Suppress(';'))\
            .setParseAction(self.external_action)

        auto_atom = (identifier('name') + Optional(constant)('value')).setParseAction(self.auto_atom_action)
        declaration_statement = (Keyword('auto').suppress() + auto_atom +
            ZeroOrMore(Literal(',').suppress() + auto_atom) + Suppress(';')).setParseAction(self.auto_action)

        # FIXME: In B, like in C, an assignment is also an expression as it is allowed inside the if/while or even in another assignment
        assignment_statement = assignment + Suppress(';')
        function_call_statement = function_call + Suppress(';')

        if_statement = (Keyword('if').suppress() + (
            Suppress('(') + (assignment | expression) + Suppress(')'))('condition') + Group(
            statement | (Suppress('{') + statement_list + Suppress('}')))('if_block') + Optional(
            Keyword('else').suppress() + (statement | (Suppress('{') + statement_list + Suppress('}'))))('else_block')) \
            .setParseAction(self.if_action)

        while_statement = Keyword('while') + Suppress('(') + (
            assignment | expression) + Suppress(')') + (statement | Suppress('{') + statement_list + Suppress('}'))

        case_statement = Keyword('case') + (integer | char) + Suppress(':') + statement_list

        switch_statement = Keyword('switch') + (assignment | expression) + (Suppress('{') + ZeroOrMore(case_statement) + Suppress('}'))

        label_statement = identifier + Suppress(':')

        statement << (
            if_statement |
            declaration_statement |
            return_statement |
            external_statement |
            assignment_statement |
            function_call_statement |
            while_statement |
            switch_statement
        )
        statement_list << ZeroOrMore(statement)

        parameter_list = delimitedList(identifier)
        function_body = Suppress('{') + statement_list + Suppress('}')
        function = ((identifier('function_name') + Group(Suppress('(') + Optional(parameter_list) + Suppress(
            ')'))('params')) + function_body).setParseAction(self.function_action)

        global_declaration = identifier + constant + Suppress(';')

        self.program = OneOrMore(function | global_declaration)

    def parse_string(self, string):
        return self.program.ignore(cStyleComment).parseString(string)

    def assignment_action(self, assignment):
        if self.debug:
            pass

        node = Node()
        node.type = 'assign'
        node.value = assignment[0]
        node.child_nodes.append(assignment[1])

        return node

    def if_action(self, if_content):
        if self.debug:
            pass

        node = IfNode()
        node.conditional_expression = if_content[0]
        node.child_nodes = list(if_content.if_block)
        node.else_nodes = list(if_content.else_block)

        return node

    def expression_action(self, value):
        if self.debug:
            pass

        node = ExpressionNode('expr', value[0])

        return node

    def function_action(self, f):
        if self.debug:
            pass

        func = Function(f.function_name, f.params)
        func.root_node.child_nodes = f[2:]

        return func

    def return_action(self, value):
        if self.debug:
            pass

        node = Node('return', value[0])

        return node

    def function_call_action(self, value):
        if self.debug:
            pass

        node = Node('funcall', value.name)
        node.child_nodes = list(value[1:])

        return node

    def external_action(self, value):
        if self.debug:
            pass

        return Node('ext', value.name)

    def auto_action(self, value):
        if self.debug:
            pass

        node = Node('decl')
        node.child_nodes = value

        return node

    def auto_atom_action(self, value):
        if self.debug:
            pass

        node = Node('autopair', value.name)
        node.child_nodes = [value.value]

        return node


if __name__ == '__main__':
    parser = Parser(debug=False)

    program = """
    /* The following function will print a non-negative number, n, to
    the base b, where 2<=b<=10,  This routine uses the fact that
    in the ANSCII character set, the digits O to 9 have sequential
    code values.  */

    printn(n,b) {
        extrn putchar;
        auto a 12, b;

        if(a=n/b) { /* assignment, not test for equality */
            printn(a, b); /* recursive */
        }
        else {

        }

        putchar(n%b + '0');
    }

    main() {
        auto a;
        printn(2,10);
        return 0;
    }
    """

    # print parser.parse_string(program)

    for function in parser.parse_string(program):
        print function
