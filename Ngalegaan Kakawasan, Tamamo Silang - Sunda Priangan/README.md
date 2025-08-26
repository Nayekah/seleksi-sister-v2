# DeezNudds

> Seleksi Asisten Laboratorium Sistem Paralel dan Terdistribusi 2025
<p align="center">
    <img src="https://github.com/user-attachments/assets/9f821cf0-87ee-4977-b762-59754039ef4c">
</p>
    <h3 align="center">Will you make this asm web server for me? :begging:</h3>

---

## About <a name="about"></a>

<p align="justify">This project implements a simple DNS system running with internal network in linux using Debian, Python, Bind9, Apache Server, Nginx, and ufw </p>

---

## Implementations <a name="algorithms"></a>

There are 4 Virtual Machines that implemented in Debian 13.0.0 (Trixie), you can refer it to https://www.debian.org/download for the .iso installations

### Virtual Machines (VMs)  
supported four Virtual Machines:

- **VM 1**  
  VM1 acts as the DNS server, with the static IP Address of 192.168.1.10

- **VM 2**  
  VM2 acts as Web Server, with the static IP Address of 192.168.1.20

- **VM 3**  
  VM3 acts as Client, the IP implemenetations depends on the user (can be manually defined or using DHCP)

- **VM 4**  
  VM4 acts as Proxy, with the static IP Address of 192.168.1.40 (and acts as a firewall too)

### Network Configurations
   ```bash
       netmask 255.255.255.0
       network 192.168.1.0
       broadcast 192.168.1.255
   ```
--- 

## How to Run <a name="how-to-run"></a>

### General Requirements
- Debian 13.0.0 (or any distro you like, just improvise)
- VirtualBox (recommended)


> [!IMPORTANT]
> This project is managed VirtualBox, so configure the network configurations based on the app you used

Next, please refer to the each VM's implementations in the VMs folder

Demo of the program: https://youtu.be/JBx2hQUj5Do

