@echo off

rem Usage example:
rem msbuild.com /m /v:minimal /fl /flp:verbosity=minimal /p:SolutionDir={path} /p:Platform={win32|x64|Any CPU|Mixed Platforms} /p:Configuration={Debug|Release} /t:{Build|Clean|Rebuild} {project.sln|project.vcxproj}"

rem Installation directory for Visual Studio releases 15.0 and upper are located by the vswhere
rem tool. Visual Studio releases 14, 12, 11 and 10 are located by checking the environment
rem variables. Visual Studio releases prior to 10 are not supported because they do not use msbuild.

rem If vswhere exists it should be located at this location path. Note vswhere only exists with
rem Visual Studio 15.0 or upper releases.
set VSWHERE="%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"

if exist %VSWHERE% (
    echo Using:
    %VSWHERE% -latest -property displayName
    %VSWHERE% -latest -property installationVersion

    for /f "usebackq tokens=*" %%i in (`%VSWHERE% -latest -property installationPath`) do (
        call "%%i\VC\Auxiliary\Build\vcvarsall.bat" x86
    )
) else if defined VS140COMNTOOLS (
    echo Using Visual Studio 2015 ^(14.0^) toolset
    call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat"
) else if defined VS120COMNTOOLS (
    echo Using Visual Studio 2013 ^(12.0^) toolset
    call "%VS120COMNTOOLS%..\..\VC\vcvarsall.bat"
) else if defined VS110COMNTOOLS (
    echo Using Visual Studio 2012 ^(11.0^) toolset
    call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat"
) else if defined VS100COMNTOOLS (
    echo Using Visual Studio 2010 ^(10.0^) toolset
    call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"
) else (
    echo Error: Visual Studio not found
    goto :eof
)

msbuild.exe %*
