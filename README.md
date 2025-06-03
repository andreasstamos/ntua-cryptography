# Computational Cryptography - 9th Semester - ECE NTUA

This repository contains my solutions to the assignments and also the project I conducted, for the *Computational Cryptography* course at
[ECE NTUA](https://www.ece.ntua.gr/en), offered during the 2024–2025 academic year.

![Banner](/assets/banner.png)

## 🔐 Project

The project is about **Publicly Verifiable & Delegatable Computing** and also contains an **Introduction to the Pairing-Based Cryptography**.

It is primarily based on the paper:

Kaoutar Elkhiyaoui *et al.*,\
**Efficient Techniques for Publicly Verifiable Delegation of Computation**\
*Proceedings of the 11th ACM on Asia Conference on Computer and Communications Security (ASIA CCS '16)*\
ACM, May 2016. [DOI: 10.1145/2897845.2897910](http://dx.doi.org/10.1145/2897845.2897910)

The introduction to pairing-based cryptography is based on the paper:

Alfred Menezes,\
**An Introduction to Pairing-Based Cryptography**\
2009. [DOI: 10.1090/conm/477/09303](http://dx.doi.org/10.1090/conm/477/09303)

Besides the schemes of this project, some interesting applications of Pairing-Based Cryptography, which are described with more details in the report, include:
- The Joux Protocol for One-Round Three-Party Key Exchange,
- The Boneh-Lynn-Shacham Signatures (BLS), known for being short (for the same equivalent bits of security).
- The Boneh-Franklin Identity-Based Encryption Scheme.

The main part of the project is about:
- A Publicly Verifiable & Delegatable Computing Scheme for **Evalution of Polynomials**.
- A Publicly Verifiable & Delegatable Computing Scheme for **Matrix-Vector Multiplication**.

**💡 Noteworthy Contribution**

For the second scheme, I have introduced an **enhancement** that **reduces the computational complexity** of certain operations and —more importantly— also
**lowers the order of magnitude of the size of certain transmitted data**.


## 🔐 Assignment 1
**Topics Covered:**

  1. CPA attack on affine ciphers (+ a KPA attack inspired by me)
  2. Ciphertext-only attack for the Vigenère cipher using Index of Coincidence and Kullback–Leibler divergence (with a programming implementation in Haskell)
  3. Theorems for Perfect Secrecy
  4. A modified One-Time-Pad and its lack of security
  5. Multiplicative One-Time-Pad and its security implications
  6. Theorems for Number Theory and Modular Arithmetic 
  7. Theorems for Abelian Groups
  8. Theorems for the Multiplicative Group of Integers Modulo n
  9. A programming implementation of the Miller-Rabin primality test (in Haskell)
  10. A logic puzzle called "Game of the Iron Throne" with a group-theoretic twist.

## 🔐 Assignment 2
**Topics Covered:**
  1. Principal Square Root of Blum Integers
  2. Security of a modified DES-X protocol
  3. Weak and Semi-Weak Keys of DES
  4. Ciphertext Collisions on Cipher Block Chaining (CBC) Mode of Operation
  5. Constant Salting on Block Ciphers and its security implications
  6. The RC4 Pseudorandom Number Generator Exploit
  7. Theorems for Psuedorandom Functions (in the cryptographic sense)
  8. Maximization of the Period of the Blum-Blum-Shub PRNG
  9. A programming implementation of the Blum-Blum-Shub PRNG with Maximum Period (in Haskell + randomized QuickCheck tests)
  10. Impossibility of Collision Resistance for XOR-Homomorphic Hash Functions
  11. Security of Merkle Trees
  12. CPA and CCA Security of a modified CBC and OFB Modes of Operation

## 🔐 Assignment 3
**Topics Covered:**
  1. Exploit of a modified RSA encryption scheme
  2. Collision Resistant Hash Function based on the computational hardness of the RSA Key Inversion Computational Problem
  3. Hardness of Square Diffie-Hellman Computational Problem and Inverse Diffie-Hellman Computational Problem
  4. RSA Decryption Attack using the Location function as an Oracle (+ a programming implementation)
  5. Extending the Legendre-Symbol attack for the Decisional Diffie-Hellman Problem to the Discrete Logarithm Problem (explaining why it would not work)
  6. ElGamal-like encryption scheme and its security implications
  7. Sigma Protocol based on then Pedersen Commitment Scheme - Completeness, Special Soundness, Honest Verifier Zero Knowledge, Witness Indistinguishability
  8. Sigma Protocol for a Zero Knowledge Proof of the cleartext of a public ciphertext.
  9. Attack for a modified Schnorr Protocol (in the Non-Interactive edition) - with a proof that this the best attack that can be constructed.
  10. A programming implementation of the Non-Interactive Schnorr Protocol - with ideas from RFC 8235 (for Schnorr protocol) and NIST FIPS186-4 (for the group generation)
  11. Attack for a modified ElGamal Signature Scheme

---

Copyright © 2025 Andreas Stamos. All rights reserved.
