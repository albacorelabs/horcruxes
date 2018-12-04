# Albacore - Horcruxes

> ## "Well, you split your soul, you see, and hide part of it in an object outside the body. Then, even if one’s body is attacked or destroyed, one cannot die, for part of the soul remains earthbound and undamaged."


Horcruxes is a threshold secret sharing scheme over Elliptic Curves. A secret can be split into **n** total parts with **m** (where m <= n) parts required to reconstruct the secret.

This repo is currently a **work in progress**.

## TODO 
* Explore bilinear pairings to remove trust in secret dealer
* Refine reconstruction algorithm for secret sharing

# Requirements and testing
- Install Haskell and Stack
- git clone git@github.com:albacorelabs/Horcruxes.git
- cd Horcruxes 
- Build and Test:  `stack build --test` , Note: tests for reconstructing secrets currently takes a medium amount of time as arbitraty instances can create secret splits > 40.



# How to use
## Inputs:
- S - A secret in the form of a point on the elliptic curve
- m - The threshold signatures required for reconstrucing the secret
- n - The total number of horcruxes the secret needs to be split into.

## Usage
- #### Splitting a secret
    - `split_horcrux m n S` will return an array of [(**Integer**, **Point**)] to be distributed to the various secret holders. Integer is required to understand the **f(i)** required for reconstruction and **Point** represents the secret shares. 

- #### Reconstrucing a secret
    - `reconstruct_horcrux [(Integer,Point)]` will return the reconstruct the original secret, S.

- #### Horcrux Helpers
    - `compress_horcrux Point` will return the compressed hex-encoded bytestring of the Point. It will discard the y-coord and append the bytes 02 or 03 depending on the parity of the y-coord.
  
    - `decompress_horcrux Cruve ByteString` will return point corresponding to the compressed bytestring passed in. It currently uses [Cipolla's Algorithm](https://en.wikipedia.org/wiki/Cipolla%27s_algorithm) for determining y in the equation y^2 = x (mod p). There is also the potential to use the [Tonelli-Shanks Algorithm](https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm)  although more investigation is required to understand if/when it is more optimal.
