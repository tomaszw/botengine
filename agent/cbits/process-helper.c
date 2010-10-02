#include <windows.h>
#include <psapi.h>
#include <string.h>
#include <stdio.h>

static const char *file_basename(const char *fname)
{
  const char *cur, *next;
  cur = next = fname;
  for (;;) {
    next = strchr(next, '\\');
    if (!next) break;
    cur = ++next;
  }
  return cur;
}

HMODULE find_module_by_name(HANDLE process, const char *name)
{
  HMODULE mods[1024];
  DWORD cb_needed;
  if (EnumProcessModules(process, mods, sizeof(mods), &cb_needed)) {
    DWORD i;
    for (i=0; i<cb_needed/sizeof(HMODULE); ++i) {
      char fname[256];
      GetModuleFileNameExA(process, mods[i], fname, sizeof(fname));
      //printf("%x = %s\n",mods[i], fname);
      const char *cur, *next;
      cur = next = fname;
      for (;;) {
        next = strchr(next, '\\');
        if (!next) break;
        cur = ++next;
      }
      if (strcmp(cur, name) == 0) {
        return mods[i];
      }
    }
  }
  return 0;
}

typedef struct {
  const char *caption;
  HWND *result;
} find_data;

static BOOL CALLBACK find_window_proc( HWND hwnd, LPARAM data_ )
{
  find_data *data = (find_data*) data_;
  char caption[512];
  GetWindowTextA(hwnd,caption,sizeof(caption));
  if (strcmp(caption,data->caption) == 0) {
    *data->result = hwnd;
    return FALSE;
  }
  return TRUE;
}

HWND find_window( const char *caption )
{
  HWND hwnd = NULL;
  find_data data;
  data.caption = caption;
  data.result = &hwnd;
  EnumWindows( find_window_proc, (LPARAM) &data);
  return hwnd;
}

HANDLE open_process_by_window_name( const char *name )
{
  printf("ZONK\n"); fflush(stdout);
  HWND hwnd = find_window(name);
  printf("window is %x\n", hwnd); fflush(stdout);
  if (hwnd) {
    DWORD process_id;
    HANDLE process;
    GetWindowThreadProcessId(hwnd, &process_id);
    process = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | PROCESS_VM_WRITE | PROCESS_VM_OPERATION, FALSE, process_id );
    printf("proc is %x\n", process); fflush(stdout);
    return process;
  }
  return 0;
}

void read_process_memory( HANDLE process, DWORD addr, DWORD len, void *data )
{
  SIZE_T num_read;
  ReadProcessMemory( process, (LPVOID*) addr, data, len, &num_read );
}


BOOL patch_is_debugger_present(HANDLE process)
{
  DWORD debpr_offset = 0x13093;    
  //BYTE code[] = { 0xb8, 0x01, 0x00, 0x00, 0x00, 0xc3 };
  BYTE code[] = { 0x64, 0xa1, 0x18, 0x00, 0x00, 0x00, 0x8b, 0x40, 0x30, 0xc6, 0x40, 0x02,
    0x00, 0x0f, 0xb6, 0x40, 0x02, 0xc3 };
      

  HMODULE kernel32 = find_module_by_name(process, "kernel32.dll");
  LPVOID write_ptr = (BYTE*)kernel32 + debpr_offset;
  SIZE_T written;
  if (!WriteProcessMemory(process, write_ptr, code, 18, &written)) {
    return FALSE;
  }
  printf("written %d bytes to %x\n", written, write_ptr);
  return TRUE;
}
