import imlib
import utility

def command_line(args):
    if 'click' in args:
        x = int(args['x'])
        y = int(args['y'])

        imlib.quick_click(x, y)
    elif 'long_click' in args:
        x = int(args['x'])
        y = int(args['y'])

        imlib.click(x, y)

if __name__ == '__main__':
    command_line(utility.command_line_args())
