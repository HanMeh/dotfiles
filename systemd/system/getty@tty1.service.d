[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin your_username --noclear %I $TERM



-----------------
sudo systemctl disable ly
sudo systemctl disable gdm
