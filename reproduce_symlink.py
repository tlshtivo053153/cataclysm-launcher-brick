#!/usr/bin/env python3
import os
import shutil
import subprocess

def setup():
    if os.path.exists("test_sandbox"):
        shutil.rmtree("test_sandbox")
    if os.path.exists("test_global_fonts"):
        shutil.rmtree("test_global_fonts")

    os.makedirs("test_sandbox/font")
    os.makedirs("test_global_fonts")
    
    with open("test_sandbox/font/testfile.txt", "w") as f:
        f.write("I am a file in a directory")

def run_test():
    sandbox_font = "test_sandbox/font"
    global_fonts = os.path.abspath("test_global_fonts")
    
    print(f"Initial state: {sandbox_font} exists: {os.path.exists(sandbox_font)}, is link: {os.path.islink(sandbox_font)}")
    
    # Simulate Haskell logic
    # 1. Check if symlink
    if os.path.islink(sandbox_font):
        os.remove(sandbox_font)
        
    # 2. Check if dir
    if os.path.isdir(sandbox_font) and not os.path.islink(sandbox_font):
        print("Removing directory recursively...")
        shutil.rmtree(sandbox_font)
        
    # 3. Create symlink
    print(f"Creating symlink from {sandbox_font} to {global_fonts}")
    os.symlink(global_fonts, sandbox_font)
    
    print(f"Final state: {sandbox_font} exists: {os.path.exists(sandbox_font)}, is link: {os.path.islink(sandbox_font)}")
    print(f"Target: {os.readlink(sandbox_font)}")

if __name__ == "__main__":
    setup()
    run_test()
