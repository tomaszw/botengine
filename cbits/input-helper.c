#define _WIN32_WINNT 0x0403

#include <windows.h>

void key_down(int vkey)
{
  INPUT inp;
  inp.type = INPUT_KEYBOARD;
  inp.ki.dwFlags = 0;
  inp.ki.wVk = vkey;
  inp.ki.time = 0;
  SendInput(1, &inp, sizeof(inp));
}

void key_up(int vkey)
{
  INPUT inp;
  inp.type = INPUT_KEYBOARD;
  inp.ki.dwFlags = 0;
  inp.ki.wVk = vkey;
  inp.ki.time = 0;
  inp.ki.dwFlags = KEYEVENTF_KEYUP;
  SendInput(1, &inp, sizeof(inp));
}

void mousebutton_down(int b)
{
  INPUT inp;
  inp.type = INPUT_MOUSE;
  inp.mi.mouseData = 0;
  inp.mi.dwFlags = b ? MOUSEEVENTF_RIGHTDOWN : MOUSEEVENTF_LEFTDOWN;
  inp.mi.time = 0;
  inp.mi.dwExtraInfo = 0;
  SendInput(1, &inp, sizeof(inp));
}

void mousebutton_up(int b)
{
  INPUT inp;
  inp.type = INPUT_MOUSE;
  inp.mi.mouseData = 0;
  inp.mi.dwFlags = b ? MOUSEEVENTF_RIGHTUP : MOUSEEVENTF_LEFTUP;
  inp.mi.time = 0;
  inp.mi.dwExtraInfo = 0;
  SendInput(1, &inp, sizeof(inp));
}


void mouse_move(int dx, int dy)
{
  INPUT inp;
  inp.type = INPUT_MOUSE;
  inp.mi.dx = dx;
  inp.mi.dy = dy;
  inp.mi.mouseData = 0;
  inp.mi.dwFlags = MOUSEEVENTF_MOVE; 
  inp.mi.time = 0;
  inp.mi.dwExtraInfo = 0;
  SendInput(1, &inp, sizeof(inp));
}

void mouse_set_cursor_pos(HWND hwnd, int x, int y)
{
  POINT lt;
  lt.x = x;
  lt.y = y;

  ClientToScreen(hwnd, &lt);
  SetCursorPos(lt.x, lt.y);
}

void mouse_set_cursor_pos_perc(HWND hwnd, double px, double py)
{
  RECT rc;
  GetClientRect(hwnd, &rc);
  int w = rc.right - rc.left;
  int h = rc.bottom - rc.top;
  double x = (double)w / 100 * px;
  double y = (double)h / 100 * py;
  mouse_set_cursor_pos(hwnd, (int)x,(int)y);
}
