import argparse
import struct


def bytes_from_file(file_name, chunksize):
    with open(file_name, "rb") as file:
        while True:
            chunk = file.read(chunksize)
            if chunk:
                yield chunk
            else:
                break


if __name__ == "__main__":
    # Usage: python3 src/main/python/parse_bytes.py --input "src/main/java/brbo/fuzz/blazer_login_unsafe/fuzzer-out/afl/queue/id:000013,src:000000,op:havoc,rep:16,+partition,+delta"
    parser = argparse.ArgumentParser(description="Parse bytes from a file into a sequence of shorts.")
    parser.add_argument(
        "--input", type=str, required=True, help="The input file to parse"
    )
    parser.add_argument(
        "--max", type=int, required=False, default=30, help="The max value of the parsed short number"
    )
    parser.add_argument(
        "--min", type=int, required=False, default=1, help="The min value of the parsed short number"
    )
    args = parser.parse_args()

    byte_array = []
    chunksize = 2
    for chunk in bytes_from_file(file_name=args.input, chunksize=chunksize):
        # print(chunk)
        byte_array.extend(chunk)
    print(f"Byte array (split into chunks of size {chunksize}): {byte_array} (length {len(byte_array)})")
    if len(byte_array) % 2 == 1:
        print(f"Remove the last element from the byte array (to ensure successful parsing)")
        byte_array.pop()
    count = int(len(byte_array) / 2)
    # https://docs.python.org/3/library/struct.html
    # big-endian, short type
    parsed = struct.unpack('>' + 'h' * count, bytes(byte_array))
    print(f"Parsed raw shorts: {parsed}")
    results = []
    for short_number in parsed:
        # The following operations must mirror the driver code that parses a sequence of bytes into (non-negative) shorts
        short_number = short_number if short_number >= 0 else -short_number
        short_number = short_number % (args.max - args.min + 1) + args.min
        results.append(short_number)
    print(f"Parsed shorts (between [{args.min}, {args.max}]): {results}")
