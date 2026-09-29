# Installing Fedora Sway Spin on Lenovo ThinkPad X1 Carbon (3rd Gen)

A step-by-step guide to installing and configuring the **Fedora Sway Spin** on a **Lenovo ThinkPad X1 Carbon (3rd Gen)** laptop.

---

## 1. Prepare the Installation Media

* **Download** the official [Fedora Sway Spin ISO](https://fedoraproject.org).
* **Flash** the ISO to a blank USB flash drive (8GB or larger) using a tool like [Fedora Media Writer](https://getfedora.org), BalenaEtcher, or `dd`.

---

## 2. Configure BIOS Settings

1. **Power off** your ThinkPad X1 Carbon completely.
2. **Turn on** the laptop and repeatedly tap the `F1` key to enter the BIOS/UEFI setup utility.
3. **Disable Secure Boot**: Navigate to the **Security** tab, select **Secure Boot**, and change it to **Disabled**. *(Note: This ensures maximum compatibility with older UEFI implementations running Wayland/Sway).*
4. **Verify UEFI Mode**: Go to the **Startup** tab and ensure **UEFI Only** or **UEFI First** is selected. Avoid Legacy/CSM mode for cleaner hardware support.
5. **Save and Exit**: Press `F10` to save your changes and reboot.

---

## 3. Boot into the Live Environment

1. **Insert** the bootable USB drive into an available USB port.
2. **Power on** the laptop and immediately tap the `F12` key to open the temporary Boot Menu.
3. **Select** your USB flash drive from the list (usually prefixed with *UEFI:*).
4. **Choose** the option to test the media and start the Fedora Sway live environment.

---

## 4. Run the Installer

1. **Launch the installer**: Once the Sway desktop loads, open the application launcher (typically `Mod + d` or `Super + d`) and select the Anaconda Installer, or open a terminal and run the installation script.
2. **Configure Partitioning**: Select your target internal SSD. The default automated layout with Btrfs is recommended for Fedora.
3. **User Setup**: Create your user account. Ensure you check the option to **grant administrative privileges** (adds user to the `wheel` group).
4. **Install**: Review the summary and begin the installation process.

---

## 5. Post-Installation & X1 Carbon Gen 3 Tweaks

After the installation finishes, reboot the system and remove your USB drive.

### System Update
Connect to your network (via `nm-applet` in the status bar or using `nmcli` in the terminal) and run a full system update:
```bash
sudo dnf upgrade --refresh
```

### TrackPoint and Touchpad Configuration
The X1 Carbon Gen 3 features a clickpad and physical TrackPoint buttons. You can fine-tune their behavior by adding the following snippet to your Sway configuration file (`~/.config/sway/config`):

```sway
# Input device configuration
input "type:touchpad" {
    tap enabled
    natural_scroll enabled
    dwt enabled # Disable-while-typing
}

input "type:pointer" {
    accel_profile "flat"
    pointer_accel 0.0
}
```

---

## Troubleshooting & Support

If you encounter any issues with hardware compatibility, power management, or multimedia keys, please feel free to open an issue or submit a pull request!

---------------------------------
---------------------------------


# Installing Fedora Sway Spin on Lenovo ThinkPad X1 Carbon (3rd Gen)

A step-by-step guide to installing, optimizing, and configuring the **Fedora Sway Spin** on a **Lenovo ThinkPad X1 Carbon (3rd Gen)** laptop.

---

## 1. Prepare the Installation Media

* **Download** the official [Fedora Sway Spin ISO](https://fedoraproject.org).
* **Flash** the ISO to a blank USB flash drive (8GB or larger) using a tool like [Fedora Media Writer](https://getfedora.org), BalenaEtcher, or `dd`.

---

## 2. Configure BIOS Settings

1. **Power off** your ThinkPad X1 Carbon completely.
2. **Turn on** the laptop and repeatedly tap the `F1` key to enter the BIOS/UEFI setup utility.
3. **Disable Secure Boot**: Navigate to the **Security** tab, select **Secure Boot**, and change it to **Disabled**. *(Note: This ensures maximum compatibility with older UEFI implementations running Wayland/Sway).*
4. **Verify UEFI Mode**: Go to the **Startup** tab and ensure **UEFI Only** or **UEFI First** is selected. Avoid Legacy/CSM mode for cleaner hardware support.
5. **Save and Exit**: Press `F10` to save your changes and reboot.

---

## 3. Boot into the Live Environment

1. **Insert** the bootable USB drive into an available USB port.
2. **Power on** the laptop and immediately tap the `F12` key to open the temporary Boot Menu.
3. **Select** your USB flash drive from the list (usually prefixed with *UEFI:*).
4. **Choose** the option to test the media and start the Fedora Sway live environment.

---

## 4. Run the Installer

1. **Launch the installer**: Once the Sway desktop loads, open the application launcher (typically `Mod + d` or `Super + d`) and select the Anaconda Installer, or open a terminal and run the installation script.
2. **Configure Partitioning**: Select your target internal SSD. The default automated layout with Btrfs is recommended for Fedora.
3. **User Setup**: Create your user account. Ensure you check the option to **grant administrative privileges** (adds user to the `wheel` group).
4. **Install**: Review the summary and begin the installation process.

---

## 5. Post-Installation & X1 Carbon Gen 3 Tweaks

After the installation finishes, reboot the system and remove your USB drive.

### System Update
Connect to your network (via `nm-applet` in the status bar or using `nmcli` in the terminal) and run a full system update:
```bash
sudo dnf upgrade --refresh
```

### TrackPoint and Touchpad Configuration
The X1 Carbon Gen 3 features a clickpad and physical TrackPoint buttons. You can fine-tune their behavior by adding the following snippet to your Sway configuration file (`~/.config/sway/config`):

```sway
# Input device configuration
input "type:touchpad" {
    tap enabled
    natural_scroll enabled
    dwt enabled # Disable-while-typing
}

input "type:pointer" {
    accel_profile "flat"
    pointer_accel 0.0
}
```

---

## 6. Power Management Optimization (TLP)

Fedora Sway comes with `power-profiles-daemon` by default, but ThinkPads gain significantly better battery life and battery health preservation using TLP.

1. **Install TLP**:
   ```bash
   sudo dnf remove power-profiles-daemon
   sudo dnf install tlp tlp-rdw
   ```
2. **Enable the Service**:
   ```bash
   sudo systemctl enable tlp --now
   ```
3. **Set Battery Charge Thresholds** (Crucial for extending the life of your X1 Carbon battery if it stays plugged in often):
   Open the configuration file (`sudo nano /etc/tlp.conf`) or create a drop-in file under `/etc/tlp.d/00-battery.conf` and append the following lines:
   ```text
   # Stop charging at 80%, start recharging when it drops below 75%
   START_CHARGE_THRESH_BAT0=75
   STOP_CHARGE_THRESH_BAT0=80
   ```

---

## 7. Custom Multimedia & Function Keybindings

Sway requires manual or environment-specific bindings to link your keyboard's hardware multimedia keys to audio and brightness backlights. Fedora Sway comes with tools like `brightnessctl` and `wireplumber` (via `wpctl`) pre-installed.

Add the following to your `~/.config/sway/config` file to activate your hardware keys:

```sway
# Volume Control (via WirePlumber)
bindsym XF86AudioRaiseVolume exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+
bindsym XF86AudioLowerVolume exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
bindsym XF86AudioMute        exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
bindsym XF86AudioMicMute     exec wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle

# Screen Brightness Control (via brightnessctl)
bindsym XF86MonBrightnessUp   exec brightnessctl set +5%
bindsym XF86MonBrightnessDown exec brightnessctl set 5%-
```

*Note: If your keys don't register automatically, make sure **Fn Lock** isn't overriding them (toggle via `Fn + Esc`).*

---

## Troubleshooting & Support

If you encounter any issues with hardware compatibility, power management, or multimedia keys, please feel free to open an issue or submit a pull request!
