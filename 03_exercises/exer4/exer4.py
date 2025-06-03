from Crypto.PublicKey import RSA
import Crypto.Random.random
import tqdm

L = 1024
NUM_TESTS = 10


def setup():
    key = RSA.generate(L)
    enc = lambda m: pow(m, key.e, key.n)
    loc = lambda c: pow(c, key.d, key.n) > key.n // 2
    return key.public_key(), enc, loc


def adversary(pk, enc, loc, c):
    c2 = enc(2)
    i = 0
    for _ in tqdm.tqdm(range(pk.n.bit_length()), position=1):
        b = loc(c)
        c = (c2 * c) % pk.n
        if b == True:
            i = 2 * i + 1
        else:
            i = 2 * i
    return ((i + 1) * pk.n) >> pk.n.bit_length()


def test():
    pk, enc, loc = setup()
    m = Crypto.Random.random.getrandbits(L) % pk.n
    c = enc(m)
    m1 = adversary(pk, enc, loc, c)
    assert m == m1


if __name__ == "__main__":
    for i in tqdm.tqdm(range(1, NUM_TESTS + 1), position=0):
        test()
    print("TESTS OK")
