#!/bin/sh
iptables -P INPUT DROP
iptables -P OUTPUT DROP
iptables -A INPUT -i lo -j ACCEPT 
iptables -A INPUT -p tcp -m tcp --dport 22 -j ACCEPT 
iptables -A OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
iptables -A OUTPUT -p tcp -m tcp --dport 10080 -j ACCEPT 
iptables -A OUTPUT -o lo -j ACCEPT 
iptables -A OUTPUT -p tcp --sport 10080 -m state --state ESTABLISHED -j ACCEPT
iptables -A INPUT -m state  --state ESTABLISHED,RELATED -j ACCEPT
