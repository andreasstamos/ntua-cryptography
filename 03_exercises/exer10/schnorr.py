import Crypto.Random.random
import hashlib
import tqdm

# ideas from RFC 8235 (for Schnorr protocol) and NIST FIPS186-4 (for the group generation)

MESSAGE_LENGTH = 16384  # arbitrary size
NUM_TESTS = 100

# suggested values from NIST FIPS186-4 paragraph 4.2
N = 256
L = 2048


def groupelem_bytes(x):
    return x.to_bytes(length=L // 8, byteorder="little")


def schnorr_prove(p, q, g, x, gx, m):
    t = Crypto.Random.random.randint(0, q - 1)  # Z_q
    gt = pow(g, t, p)

    c = hashlib.sha512(
        groupelem_bytes(g) + groupelem_bytes(gx) + groupelem_bytes(gt) + m
    ).digest()
    c = int.from_bytes(c, byteorder="little")
    c = c % p

    s = (t + c * x) % p

    return c, s


def schnorr_verify(p, q, g, gx, m, sig):
    (c, s) = sig

    gt = (pow(g, s, p) * pow(gx, -c, p)) % p

    c1 = hashlib.sha512(
        groupelem_bytes(g) + groupelem_bytes(gx) + groupelem_bytes(gt) + m
    ).digest()
    c1 = int.from_bytes(c1, byteorder="little")
    c1 = c1 % p

    return c == c1


def get_schnorr_primes(n, l):
    # ideas from NIST FIPS186-4 appendix A.1
    while True:
        q = Crypto.Util.number.getPrime(n)

        x = Crypto.Util.number.getRandomNBitInteger(l)
        p = x - (x % (2 * q) - 1)

        if p.bit_length() < l:
            continue
        if Crypto.Util.number.isPrime(p):
            return (p, q)


def get_schnorr_g(p, q):
    # ideas from NIST FIPS186-4 appendix A.2.1
    e = (p - 1) // q  # p == q*r+1
    while True:
        h = Crypto.Random.random.randint(2, p - 2)  # Z_q
        g = pow(h, e, p)
        if g != 1:
            return g


def test(p, q, g):
    for _ in tqdm.tqdm(range(NUM_TESTS)):
        x = Crypto.Random.random.randint(0, q - 1)
        gx = pow(g, x, p)

        m = Crypto.Random.get_random_bytes(MESSAGE_LENGTH)

        sig = schnorr_prove(p, q, g, x, gx, m)

        status = schnorr_verify(p, q, g, gx, m, sig)

        assert status == True


if __name__ == "__main__":
    print("Generating primes p, q...")
    p, q = get_schnorr_primes(N, L)

    print("Genrating generator g...")
    g = get_schnorr_g(p, q)

    print(f"Random testing ({NUM_TESTS} tests) for random private keys and messages...")
    test(p, q, g)

    print("TESTS OK")
