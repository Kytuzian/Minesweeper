import os
import time

try:
    import win32gui
except:
    pass

import sys

import pymouse

import matplotlib.pyplot as plt
import numpy as np

import utility

from PIL import Image
from PIL import ImageGrab

def command_line(args):
    if 'screenshot' in args:
        fname = args.get('fname', 'temp.png')

        if 'winname' in args:
            screenshot_by_name(args['winname'], fname)
        else:
            take_screenshot(fname)

def put_data(im, data, w, h):
    data = reduce(lambda a, b: a + b, data)
    for x in xrange(w):
        for y in xrange(h):
            im.putpixel((x, y), int(data[x * h + y] * 255))

def scatterplot(points, fname, size=(1366, 768)):
    im = Image.new("L", size)
    if isinstance(points, dict):
        for (y, x), v in points.items:
            im.putpixel((x, y), v * 255)
    else:
        for y, x in points:
            im.putpixel((x, y), 255)
    im.save(fname)

def mouse_position():
    return pymouse.PyMouse().position()

def screen_size():
    return pymouse.PyMouse().screen_size()

def move(x, y):
    pymouse.PyMouse().move(x, y)

# Detect movement causes the program to cancel the click if the mouse has moved since the last call
last_mouse_pos = (-1, -1)
def mouse_moved(x, y):
    global last_mouse_pos
    old_x, old_y = pymouse.PyMouse().position()
    last_x, last_y = last_mouse_pos

    if last_x > -1 and last_y > -1:
        if distance((last_x, last_y), (old_x, old_y)) > 4:
            # print(last_x, last_y, old_x, old_y, x, y)
            return True

    return False

def quick_click(x=-1, y=-1, mousebutton=1, detect_movement=True):
    if detect_movement and mouse_moved(x, y):
        raise Exception('Mouse moved!')

    if x < 0 or y < 0:
        x, y = pymouse.PyMouse().position()
    pymouse.PyMouse().click(x, y, mousebutton)

    last_mouse_pos = pymouse.PyMouse().position()
    # pymouse.PyMouse().move(old_x, old_y)

def click(x=-1, y=-1, mousebutton=1, detect_movement=True):
    if detect_movement and mouse_moved(x, y):
        raise Exception('Mouse moved!')

    if x < 0 or y < 0:
        x, y = pymouse.PyMouse().position()
    pymouse.PyMouse().press(x, y, mousebutton)
    time.sleep(0.5)
    pymouse.PyMouse().release(x, y, mousebutton)
    time.sleep(1)

# From the accepted answer on http://stackoverflow.com/questions/3260559/how-to-get-a-window-or-fullscreen-screenshot-in-python-3k-without-pil
def screenshot_by_name(name, fname='temp.png'):
    if utility.is_windows():
        toplist, winlist = [], []
        def enum_cb(hwnd, results):
            winlist.append((hwnd, win32gui.GetWindowText(hwnd)))
        win32gui.EnumWindows(enum_cb, toplist)

        for _, title in winlist:
            if title != '':
                print(title)
        proc = [(hwnd, title) for hwnd, title in winlist if name in title.lower()]

        if len(proc) > 0:
            # just grab the hwnd for first window matching the name
            proc = proc[0]
            hwnd = proc[0]

            win32gui.SetForegroundWindow(hwnd)
            bbox = win32gui.GetWindowRect(hwnd)

            print(bbox)
            img = ImageGrab.grab(bbox)

            img.save(fname)

            return img
        else:
            raise Exception('No window with that title.')
    else:
        raise Exception('screenshot_by_name only available on windows.')

def read_image(file_name):
    return plt.imread(file_name)

def take_screenshot(fname='temp.png'):
    if utility.is_osx():
        os.system('screencapture -x {}'.format(fname))
    else:
        import wx
        wxApp = wx.App()  # Need to create an App instance before doing anything
        screen = wx.ScreenDC()
        size = screen.GetSize()
        bmp = wx.EmptyBitmap(size[0], size[1])
        mem = wx.MemoryDC(bmp)
        mem.Blit(0, 0, size[0], size[1], screen, 0, 0)
        del mem  # Release bitmap
        bmp.SaveFile('{}'.format(fname), wx.BITMAP_TYPE_PNG)

    return read_image('{}'.format(fname))

def enter_url(url, t=0.1):
    click(274, 78) #Address bar
    typeln(url, t=t)

    time.sleep(3)

def press_key(key, t=0.1):
    if utility.is_osx():
        if key == '<return>':
            cmd = """
                osascript -e 'tell application "System Events" to keystroke return'
                """
        else:
            cmd = """
                osascript -e 'tell application "System Events" to keystroke "{}"'
                """.format(key)
        os.system(cmd)
    elif utility.is_windows():
        for c in winutil.process_key_windows(key):
            winutil.key_event(c)
    else:
        raise Exception('Keyboard presses not supported on operating systems other than Windows and OS X.')

def typeln(to_type, t=0.1):
    press_key(to_type, t=t)
    press_key('<return>', t=t)

def crop_image(im, (xmin, xmax, ymin, ymax)):
    copy = []

    for iy, y in enumerate(im[ymin:ymax]):
        copy.append([])
        for x in y[xmin:xmax]:
            copy[-1].append(x)

    return np.array(copy)

def rough_color_match(a, b, tolerance):
    if tolerance == 0.0:
        return tuple(a) == tuple(b)

    a = map(float, (utility.clamp(a[0], 0.004, 1), utility.clamp(a[1], 0.004, 1), utility.clamp(a[2], 0.004, 1)))
    b = map(float, (utility.clamp(b[0], 0.004, 1), utility.clamp(b[1], 0.004, 1), utility.clamp(b[2], 0.004, 1)))

    drMatch = int(utility.rough_match(a[0], b[0], tolerance))
    dgMatch = int(utility.rough_match(a[1], b[1], tolerance))
    dbMatch = int(utility.rough_match(a[2], b[2], tolerance))

    total = drMatch + dgMatch + dbMatch

    if total == 3:
        return True

    for ar, br in zip(utility.all_ratios(a), utility.all_ratios(b)):
        total += int(utility.rough_match(ar, br, tolerance))

        if total > 4:
            return True

    return False

def grayscale(im, verbose=0):
    res = []
    w, h = float(len(im)), float(len(im[0]))
    for iy, y in enumerate(im):
        res.append([])
        for ix, x in enumerate(y):
            res[-1].append(utility.average(x))
            if verbose > 0:
                utility.show_bar(iy * h + ix, w * h, number_limit=True, message='Grayscaling: ')
    return np.array(res, dtype=float)

def get_pixels_of_color(im, color, tolerance=0.0, verbose=0):
    res = []
    w, h = float(len(im)), float(len(im[0]))
    for iy, y in enumerate(im):
        for ix, x in enumerate(y):
            if rough_color_match(x, color, tolerance):
                res.append((iy, ix))
                if verbose > 0:
                    utility.show_bar(iy * h + ix, w * h, number_limit=True, message='Finding {} ({}): '.format(color, len(res)))

    return res

def determine_discrepancies(im1, im2, pixels, tolerance=0.0, verbose=0):
    res = []
    w, h = float(len(im1)), float(len(im1[0]))
    for iy, y in enumerate(im1):
        for ix, x in enumerate(y):
            if not rough_color_match(x, im2[iy][ix], tolerance):
                res.append((iy, ix))
            if verbose > 0:
                utility.show_bar(iy * h + ix, w * h, number_limit=True, message='Finding discrepancies ({}): '.format(len(res)))

    return res

def any_pixels_are(im, color, (xmin, ymin, xmax, ymax), tolerance=0.0):
    for y in im[ymin:ymax]:
        for x in y[xmin:xmax]:
            if rough_color_match(color, x, tolerance):
                return True

    return False

def image_match(im1, im2, (xmin, xmax, ymin, ymax), tolerance=1.0):
    matchpx = 0.0
    totalpx = 0.0
    for y1, y2 in it.izip(im1[ymin:ymax], im2):
        for x1, x2 in it.izip(y1[xmin:xmax], y2):
            totalpx += 1.0
            if tuple(x1) != tuple(x2):
                if matchpx / totalpx < tolerance:
                    return False
            else:
                matchpx += 1.0

    return True

if __name__ == '__main__':
    command_line(utility.command_line_args(sys.argv))
