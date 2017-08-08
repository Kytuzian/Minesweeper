import ctypes
from ctypes import wintypes
import time

user32 = ctypes.WinDLL('user32', use_last_error=True)

INPUT_MOUSE    = 0
INPUT_KEYBOARD = 1
INPUT_HARDWARE = 2

KEYEVENTF_EXTENDEDKEY = 0x0001
KEYEVENTF_KEYUP       = 0x0002
KEYEVENTF_UNICODE     = 0x0004
KEYEVENTF_SCANCODE    = 0x0008

MAPVK_VK_TO_VSC = 0

# C struct definitions

wintypes.ULONG_PTR = wintypes.WPARAM

class MOUSEINPUT(ctypes.Structure):
    _fields_ = (("dx",          wintypes.LONG),
                ("dy",          wintypes.LONG),
                ("mouseData",   wintypes.DWORD),
                ("dwFlags",     wintypes.DWORD),
                ("time",        wintypes.DWORD),
                ("dwExtraInfo", wintypes.ULONG_PTR))

class KEYBDINPUT(ctypes.Structure):
    _fields_ = (("wVk",         wintypes.WORD),
                ("wScan",       wintypes.WORD),
                ("dwFlags",     wintypes.DWORD),
                ("time",        wintypes.DWORD),
                ("dwExtraInfo", wintypes.ULONG_PTR))

    def __init__(self, *args, **kwds):
        super(KEYBDINPUT, self).__init__(*args, **kwds)
        # some programs use the scan code even if KEYEVENTF_SCANCODE
        # isn't set in dwFflags, so attempt to map the correct code.
        if not self.dwFlags & KEYEVENTF_UNICODE:
            self.wScan = user32.MapVirtualKeyExW(self.wVk,
                                                 MAPVK_VK_TO_VSC, 0)

class HARDWAREINPUT(ctypes.Structure):
    _fields_ = (("uMsg",    wintypes.DWORD),
                ("wParamL", wintypes.WORD),
                ("wParamH", wintypes.WORD))

class INPUT(ctypes.Structure):
    class _INPUT(ctypes.Union):
        _fields_ = (("ki", KEYBDINPUT),
                    ("mi", MOUSEINPUT),
                    ("hi", HARDWAREINPUT))
    _anonymous_ = ("_input",)
    _fields_ = (("type",   wintypes.DWORD),
                ("_input", _INPUT))

LPINPUT = ctypes.POINTER(INPUT)

def _check_count(result, func, args):
    if result == 0:
        raise ctypes.WinError(ctypes.get_last_error())
    return args

user32.SendInput.errcheck = _check_count
user32.SendInput.argtypes = (wintypes.UINT, # nInputs
                             LPINPUT,       # pInputs
                             ctypes.c_int)  # cbSize

# Functions
# Get the hex code for a command key
def process_key_windows(key):
    if len(key) > 1:
        while len(key) > 0:
            val = ''
            if key[0] == '<':
                for i, c in enumerate(key):
                    if c == '>':
                        val = key[1:i]
                        break

                if val == '':
                    raise Exception('No closing angle bracket found in {}!'.format(key))

                key = key[i + 1:]
            else:
                val = key[0]
                key = key[1:]

            yield keys[val]
    else:
        yield keys[key]

def press_key(hex_key_code):
    x = INPUT(type=INPUT_KEYBOARD,
              ki=KEYBDINPUT(wVk=hex_key_code))
    user32.SendInput(1, ctypes.byref(x), ctypes.sizeof(x))

def release_key(hex_key_code):
    x = INPUT(type=INPUT_KEYBOARD,
              ki=KEYBDINPUT(wVk=hex_key_code,
                            dwFlags=KEYEVENTF_KEYUP))
    user32.SendInput(1, ctypes.byref(x), ctypes.sizeof(x))

def key_event(hex_key_code, t=0.1):
    press_key(hex_key_code)
    time.sleep(t)
    release_key(hex_key_code)

def AltTab():
    """Press Alt+Tab and hold Alt key for 2 seconds
    in order to see the overlay.
    """
    press_key(process_key_windows('alt'))   # Alt
    press_key(process_key_windows('tab'))    # Tab
    release_key(process_key_windows('tab'))  # Tab~
    time.sleep(2)
    release_key(process_key_windows('alt')) # Alt~

# KEYS
# msdn.microsoft.com/en-us/library/dd375731
keys = {'tab': 0x09,
        'alt': 0x12,
        'lbutton': 0x01, 'rbutton': 0x02, 'mbutton': 0x04,
        'cancel': 0x03,
        'x1button': 0x05, 'x2button': 0x06,
        'backspace': 0x0008,
        'clear': 0x0C,
        'return': 0x0d,
        'shift': 0x10, 'lshift': 0xa0, 'rshift': 0xa1,
        'control': 0x11, 'lcontrol': 0xa2, 'rcontrol': 0xa3,
        'pause': 0x13,
        'capslock': 0x14,
        'escape': 0x1b,
        'space': 0x20,
        'pageup': 0x21,
        'pagedown': 0x22,
        'end': 0x23,
        'home': 0x24,
        'left': 0x25, 'up': 0x26, 'right': 0x27, 'down': 0x28,
        'print': 0x2a,
        'printscreen': 0x2c,
        'insert': 0x2d,
        'delete': 0x2e,
        '0': 0x30, '1': 0x31, '2': 0x32, '3': 0x33, '4': 0x34, '5': 0x35, '6': 0x36, '7': 0x37, '8': 0x38, '9': 0x39,
        'a': 0x41,
        'b': 0x42,
        'c': 0x43,
        'd': 0x44,
        'e': 0x45,
        'f': 0x46,
        'g': 0x47,
        'h': 0x48,
        'i': 0x49,
        'j': 0x4a,
        'k': 0x4b,
        'l': 0x4c,
        'm': 0x4d,
        'n': 0x4e,
        'o': 0x4f,
        'p': 0x50,
        'q': 0x51,
        'r': 0x52,
        's': 0x53,
        't': 0x54,
        'u': 0x55,
        'v': 0x56,
        'w': 0x57,
        'x': 0x58,
        'y': 0x59,
        'z': 0x5A,
        'leftwin': 0x5b, 'rightwin': 0x5c,
        'sleep': 0x5f,
        'num0': 0x60,
        'num1': 0x61,
        'num2': 0x62,
        'num3': 0x63,
        'num4': 0x64,
        'num5': 0x65,
        'num6': 0x66,
        'num7': 0x67,
        'num8': 0x68,
        'num9': 0x69,
        'mult': 0x6a, 'add': 0x6b, 'sub': 0x6d, 'div': 0x6f,
        'decimal': 0x6e,
        'f1': 0x70, 'f2': 0x71, 'f3': 0x72, 'f4': 0x73, 'f5': 0x74, 'f6': 0x75,
        'f7': 0x76, 'f8': 0x77, 'f9': 0x78, 'f10': 0x79, 'f11': 0x7a, 'f12': 0x7b,
        'f13': 0x7c, 'f14': 0x7d, 'f15': 0x7e, 'f16': 0x7f, 'f17': 0x80, 'f18': 0x81,
        'f19': 0x82, 'f20': 0x83, 'f21': 0x84, 'f22': 0x85, 'f23': 0x86, 'f24': 0x87,
        'numlock': 0x90, 'scrolllock': 0x91,
        'browserback': 0xa6, 'browserforward': 0xa7, 'browserrefresh': 0xA8,
        'browserstop': 0xA9, 'browsersearch': 0xaa, 'browserfavorites': 0xab,
        'browserhome': 0xac,
        'mute': 0xad,
        'volumedown': 0xae,
        'volumeup': 0xaf,
        'nexttrack': 0xb0,
        'prevtrack': 0xb1,
        'mediastop': 0xb2,
        'mediaplay': 0xb3,
        ';': 0xba,
        '+': 0xbb,
        ',': 0xbc,
        '-': 0xbd,
        '.': 0xbe,
        '/': 0xbf,
        '`': 0xc0,
        '[': 0xdb,
        '\\': 0xdc,
        ']': 0xdd,
        '\'': 0xde,
        'play': 0xfa,
        'zoom': 0xfb,
        'clear': 0xfe}
