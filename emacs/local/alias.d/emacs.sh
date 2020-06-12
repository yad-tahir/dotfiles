#/bin/sh
# Copyright (C) 2020

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

alias e="emacs"
alias ec="emacsclient -cn"
alias emacs-restart="systemctl --user restart emacs-27-vcs.service"
alias emacs-stop="systemctl --user stop emacs-27-vcs.service"
alias emacs-start="systemctl --user start emacs-27-vcs.service"
alias emacs-mask="systemctl --user mask emacs-27-vcs.service"
alias emacs-unmask="systemctl --user unmask emacs-27-vcs.service"
