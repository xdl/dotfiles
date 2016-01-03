if (Test-Path $home\_vimrc) {
	Remove-Item $home\_vimrc;
}

if (Test-Path $home\_gvimrc) {
	Remove-Item $home\_gvimrc;
}

if (Test-Path $home\.gitconfig) {
	Remove-Item $home\.gitconfig;
}

if (Test-Path $home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1) {
	Remove-Item $home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1;
}

cmd /c mklink $home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 $home\dotfiles\Microsoft.PowerShell_profile.ps1;
cmd /c mklink $home\.gitconfig $home\dotfiles\.gitconfig;
cmd /c mklink $home\_vimrc $home\dotfiles\vim\.vimrc;
cmd /c mklink $home\_gvimrc $home\dotfiles\vim\.gvimrc;
