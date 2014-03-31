/*
	BASS simple DSP test
	Copyright (c) 2000-2012 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdio.h>
#include <math.h>
#include "bass.h"

DWORD chan;	// the channel... HMUSIC or HSTREAM

HWND FindMyTopMostWindow()
{
	DWORD dwProcID = GetCurrentProcessId();
	HWND hWnd = GetTopWindow(GetDesktopWindow());
	while(hWnd)
	{
		DWORD dwWndProcID = 0;
		GetWindowThreadProcessId(hWnd, &dwWndProcID);
		if(dwWndProcID == dwProcID)
			return hWnd;            
		hWnd = GetNextWindow(hWnd, GW_HWNDNEXT);
	}
	return NULL;
 }

INT_PTR CALLBACK dialogproc(HWND h,UINT m,WPARAM w,LPARAM l)
{
	return 1;
}


__declspec(dllexport) int __cdecl initBASS() {
	if (HIWORD(BASS_GetVersion())!=BASSVERSION) {
		MessageBox(0,"An incorrect version of BASS.DLL was loaded",0,MB_ICONERROR);
		return 1;
	}
	HWND mainWindow = FindMyTopMostWindow();
	BASS_Init(-1,44100,0,mainWindow,NULL);
	return 1;
}

__declspec(dllexport) int __cdecl initBASSSound(char path[]) {

  if((chan=BASS_StreamCreateFile(FALSE,path,0,0,FALSE))==0)
	return BASS_ErrorGetCode();
}

__declspec(dllexport) int __cdecl playBASSSound( HSTREAM chan )
{
	BASS_ChannelPlay(chan,FALSE);	
	return 1;
}

__declspec(dllexport) int __cdecl stopBASSSound( HSTREAM chan ) {
	BASS_ChannelStop(chan);
}

