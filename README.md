# 🧠 AI Model Marketplace - Clarity MVP

Welcome to the **AI Model Marketplace**, a decentralized platform on Stacks where AI models are monetized and consumed using **sBTC**! 🚀

## 📜 Overview
This project implements a smart contract system for registering AI models, staking as an oracle, and paying for inference requests on-chain.

**Key Features:**
- 🤖 **Model Registry**: Developers can register models with versioning and pricing.
- 💸 **sBTC Payments**: Native sBTC support for secure and fast payments.
- 🔮 **Oracle System**: Oracles stake funds to process requests and earn fees.
- ⚡ **Security**: Built-in slashing mechanism for bad actors.

## 📂 Contracts
- `ai-marketplace.clar`: The core marketplace logic (Registration, Request, Fulfill, Slash).
- `sbtc-trait.clar`: Trait definition for sBTC interoperability.

## 🛠️ Usage

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed.
- Node.js & NPM (for testing).

### Setup
1. Clone the repository.
2. Run `clarinet check` to verify contracts.

### Interactions
**Register a Model**
```clarity
(contract-call? .ai-marketplace register-model "GPT-X" "Advanced NLP" u5000)
```

**Register as Oracle**
```clarity
(contract-call? .ai-marketplace register-oracle .sbtc-token u100000)
```

**Request Inference**
```clarity
(contract-call? .ai-marketplace request-inference .sbtc-token u1 0x...)
```

## 🧪 Testing
Run the Clarinet console to interact manually:
```bash
clarinet console
```

---
*Built with ❤️ on Stacks*
