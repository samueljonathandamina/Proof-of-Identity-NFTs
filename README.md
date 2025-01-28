# Proof-of-Identity NFTs

A Stacks blockchain implementation of identity verification using NFTs. This system enables verified identities to be represented as non-fungible tokens on the blockchain.

## Features

- Identity verification through NFTs
- One POI token per address
- Controlled verification process
- Built on Stacks blockchain using Clarity

## Contract Functions

### Verification
- `verify-address`: Allows contract owner to verify user addresses
- Only contract owner can perform verification

### NFT Operations
- `mint`: Allows verified addresses to mint their POI NFT
- `get-last-token-id`: Returns the latest token ID
- `get-token-uri`: Retrieves token metadata URI
- `get-owner`: Returns the owner of a specific token

## Error Codes
- `u100`: Owner-only operation
- `u101`: Address not verified
- `u102`: Address already has POI token

## Testing

Tests are written using Vitest and cover:
- Address verification
- NFT minting
- Error handling for unverified addresses

## Development

### Prerequisites
- Clarinet
- Node.js
- Vitest

### Running Tests
```bash
npm test
