from pyparsing import *


class Node(object):
    def __init__(self):
        self.child_nodes = []
        self.parent_node = None
        self.type = ""
        self.value = None
        self.has_block = False

    def __str__(self):
        ret = "%s\n%s\n" % (self.type, str(self.value))

        for node in self.child_nodes:
            ret += str(node) + '\n'

        return ret


class ConditionalNode(Node):
    def __init__(self):
        super(ConditionalNode, self).__init__()

        self.conditional_expression = None
        self.has_block = True


class IfNode(ConditionalNode):
    def __init__(self):
        super(IfNode, self).__init__()

        self.else_node = None
        self.has_block = True
        self.type = 'if'


class Function(object):
    def __init__(self, name, params):
        self.name = name
        self.params = params
        self.root_node = Node()
        self.root_node.type = 'root'

    def __str__(self):
        return "%s\n%s\n%s" % (self.name, str(self.params), str(self.root_node))


class CodeTree(object):
    def __init__(self):
        self.global_vars = []
        self.functions = []

        self.current_node = None

    def add_function(self, function):
        self.functions.append(function)
        self.current_node = function.root_node

    def close_node(self):
        self.current_node = self.current_node.parent_node

    def add_node(self, node):
        if self.current_node.type == 'if':
            self.current_node.conditional_expression = node
            return

        self.current_node.child_nodes.append(node)
        node.parent_node = self.current_node

        if node.has_block:
            self.current_node = node

    def __str__(self):
        ret = ''

        for function in self.functions:
            ret += str(function) + "\n"

        return ret


class Parser(object):
    def __init__(self, debug=False):
        self.code = CodeTree()
        self.debug = debug

        identifier = Word(alphas + "_", alphanums + "_")
        integer = Word(nums)
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

        arguments = delimitedList(expression('exp'))
        function_call = ((identifier('name') + FollowedBy('(')) + lpar + Optional(arguments)('args') + rpar)

        precendence = [
            (multop, 2, opAssoc.LEFT),
            (plusop, 2, opAssoc.LEFT),
            (logical_binary_operators, 2, opAssoc.LEFT)
        ]
        expression << (operatorPrecedence(function_call | constant | identifier, precendence)) \
            .setParseAction(self.expression)

        assignment = (identifier + Word('=').suppress() + expression('value')).setParseAction(self.assignment)

        # statements
        return_statement = Keyword('return') + expression + Suppress(';')
        external_statement = (Keyword('extrn').suppress() + identifier('name') + Suppress(';')).setParseAction(
            self.external)

        auto_atom = identifier + Optional(constant)
        declaration_statement = Keyword('auto') + auto_atom + ZeroOrMore(
            Literal(',') + auto_atom) + Suppress(';')

        # FIXME: In B, like in C, an assignment is also an expression as it is allowed inside the if/while or even in another assignment
        assignment_statement = assignment + Suppress(';')
        function_call_statement = function_call + Suppress(';')

        if_statement = Keyword('if').setParseAction(self.begin_if) + (
            Suppress('(') + (assignment | expression) + Suppress(')')) + Empty().setParseAction(self.if_block) + (
            statement | (Suppress('{') + statement_list + Suppress('}'))) + Empty().setParseAction(
            self.finish_block) + Optional(
            Keyword('else') + (statement | (Suppress('{') + statement_list + Suppress('}')))) \
                           .setParseAction(self.finish_block)

        while_statement = Keyword('while').setParseAction(self.begin_while) + Suppress('(') + (
            assignment | expression) + Suppress(')') + Empty().setParseAction(self.while_block) + (
                              statement | Suppress('{') + statement_list + Suppress('}')) + Empty().setParseAction(
            self.finish_block)

        statement << (
            declaration_statement |
            return_statement |
            external_statement |
            assignment_statement |
            function_call_statement |
            if_statement |
            while_statement
        )
        statement_list << OneOrMore(statement)

        parameter_list = delimitedList(identifier)
        function_body = Suppress('{') + statement_list + Suppress('}')
        function = (identifier('name') + Group(Suppress('(') + Optional(parameter_list) + Suppress(
            ')'))('params')).setParseAction(self.begin_function) + function_body + Empty().setParseAction(
            self.finish_block)

        global_declaration = identifier + constant + Suppress(';')

        self.program = OneOrMore(function | global_declaration)

    def external(self, value):
        if self.debug:
            print 'extern'
            print value.name

        node = Node()
        node.value = value.name
        node.type = 'extern'

        self.code.add_node(node)

    def expression(self, value):
        if self.debug:
            print 'expression'
            print value

        node = Node()
        node.value = value
        node.type = 'expr'

        self.code.add_node(node)

    def assignment(self, value):
        if self.debug:
            print 'assignment'
            print value

        node = Node()
        node.value = value
        node.type = 'assign'

        self.code.add_node(node)

    def begin_if(self, value):
        if self.debug:
            print 'if'

        self.code.add_node(IfNode())

    def if_block(self, value):
        pass

    def begin_while(self, value):
        pass

    def while_block(self, value):
        pass

    def else_block(self, value):
        pass

    def begin_function(self, function):
        self.code.add_function(Function(function.name, function.params))

    def finish_block(self, value):
        pass

    def parse_string(self, string):
        return self.program.ignore(cStyleComment).parseString(string)


if __name__ == '__main__':
    parser = Parser(debug=True)

    program = """
    /* The following function will print a non-negative number, n, to
  the base b, where 2<=b<=10,  This routine uses the fact that
  in the ANSCII character set, the digits O to 9 have sequential
  code values.  */

    printn(n,b) {
        extrn putchar;
        auto a;

        if(a=n/b) { /* assignment, not test for equality */
            printn(a, b); /* recursive */
        }
        putchar(n%b + '0');

    }
    """

    print parser.parse_string(program)
    print
    print parser.code
