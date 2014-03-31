/* add_basic.c

   Demonstrates creating a DLL with an exported function, the inflexible way.
*/


//#include "stdafx.h"
#include <Windows.h>
#include <string.h>
#include <stdio.h>
//#include <iostream>

__declspec(dllexport) int __cdecl Add(int a, int b)
{
  return (a + b);
}


__declspec(dllexport) int __cdecl Add2(int a, int b)
{
  return (a + b);
}


extern char command1[] = "open C:\\boing.mp3 type MPEGVideo alias 0";
extern char command2[] = "play 0 from 0";

__declspec(dllexport) int __cdecl playSound(char path[])
{

	mciSendStringA(command1, NULL, 0, 0);
	mciSendStringA(command2, NULL, 0, 0);
	return 1;
}

__declspec(dllexport) int __cdecl playSound2(char path[])
{
	PlaySound(TEXT("c:\\boing.mp3"), NULL, SND_FILENAME);
}