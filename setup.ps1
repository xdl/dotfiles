if (Test-Path $home\_vimrc) {
	Remove-Item $home\_vimrc;
}

if (Test-Path $home\_gvimrc) {
	Remove-Item $home\_gvimrc;
}

if (Test-Path $home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1) {
	Remove-Item $home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1;
}

fsutil hardlink create $home\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 $home\dotfiles\Microsoft.PowerShell_profile.ps1;
fsutil hardlink create $home\_vimrc $home\dotfiles\vim\.vimrc;
fsutil hardlink create $home\_gvimrc $home\dotfiles\vim\.gvimrc;
