#include <Windows.h>
#include <io.h>
#include <fcntl.h>
#include <stdio.h>

extern void tinyapl_setup(void) {
	_setmode(_fileno(stdin), _O_U16TEXT);
	_setmode(_fileno(stdout), _O_U16TEXT);
	_setmode(_fileno(stderr), _O_U16TEXT);
}

extern void tinyapl_read(void (*callback)(const char* restrict, size_t)) {
	HANDLE input = GetStdHandle(STD_INPUT_HANDLE);

	wchar_t buffer[1024];
	DWORD read = 0;
	ReadConsoleW(input, buffer, sizeof(buffer) / sizeof(buffer[0]), &read, NULL);
	int utf8_count = WideCharToMultiByte(CP_UTF8, 0, buffer, read, NULL, 0, NULL, NULL);
	char* result = malloc(utf8_count + 1);
	WideCharToMultiByte(CP_UTF8, 0, buffer, read, result, utf8_count, NULL, NULL);
	callback(result, utf8_count - 2);
	free(result);
}

extern void tinyapl_write(const char* restrict text, size_t size, _Bool error) {
	HANDLE output = GetStdHandle(error ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);

	int wchar_count = MultiByteToWideChar(CP_UTF8, 0, text, size, NULL, 0);
	wchar_t *buffer = malloc(sizeof(wchar_t) * (wchar_count + 1));
	MultiByteToWideChar(CP_UTF8, 0, text, size, buffer, wchar_count);
	WriteConsoleW(output, buffer, wchar_count, NULL, NULL);
	free(buffer);
}
