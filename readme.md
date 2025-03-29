# AdaScore — Cardano Decentralized Betting

This repository contains the full-stack implementation of **AdaScore**, a decentralized peer-to-peer sports betting platform built on **Cardano**.

Developed as part of the **EMURGO Cardano Developer Program (CDP)**, this project aims to eliminate the unfair advantage of traditional bookmakers by enabling trustless, direct betting between individuals.

---

## 🎯 What is AdaScore?

**AdaScore** is a decentralized betting platform that allows users to place **peer-to-peer sports bets** without intermediaries or manipulated odds.  
Built on **Cardano**, it uses smart contracts to ensure fairness, transparency, and full user control over betting terms.

---

## 💡 The Core Idea

Traditional betting platforms are centralized entities optimized to guarantee profit at the user's expense, backed by statisticians and risk models.

**AdaScore flips the model**:

- Create your own bet with your **own odds**
- Let someone else **accept it**
- The **outcome is resolved** by a trusted third-party oracle
- The platform is **non-custodial**, **trustless**, and **uncensorable**

---

## 🔐 Incentive Structure

### ✅ For Bet Creators

- Tailor bets to match your **risk appetite** and **expected return**
- Define your own odds — just ensure they’re **enticing enough** to attract an acceptor
- Create **multiple bets** to diversify and maximize expected value
- **No censorship** — even consistent winners are not limited or banned

### ✅ For Bet Acceptors

- Only accept bets with **odds you believe are in your favor**
- Can't find a good bet? **Post your own**
- **Fees are paid** by the bet creator

### ✅ For Oracles

- Delivering **accurate match results** is in the oracle’s best interest
- Integrity ensures continued usage of the platform
- Oracle behavior is **auditable** and replaceable

---

## 🧱 Tech Stack

| Layer        | Tech                            |
|--------------|----------------------------------|
| Smart Contracts | ✅ [Plutus](https://github.com/input-output-hk/plutus) + ✅ [Aiken](https://aiken-lang.org) |
| Frontend     | ✅ [Next.js](https://nextjs.org/) (React-based)          |
| Backend/API  | ✅ Google Cloud Functions          |
| Database     | ✅ Firebase (Firestore)           |
| Blockchain   | ✅ Cardano (UTxO model)            |

---

## ⚙️ How It Works

1. **Connect** your Cardano wallet
2. **Browse matches** or **create your own bet**
3. Set odds and outcome conditions
4. Wait for another user to **accept** the bet
5. Match resolves via an **oracle**, funds are automatically settled

---

## 📁 Project Structure

```
adascore-cardano-decentralized-betting/
│
├── contracts/
│   ├── plutus/        # Plutus smart contracts (original version)
│   └── aiken/         # Aiken smart contracts (preferred, cleaner syntax)
│
├── frontend/          # Next.js app (UI + server-side API routes)
│
├── firebase/          # Cloud Functions and Firebase config
│
├── docs/              # Diagrams, rationales, PPTs
│
└── README.md
```

---

## 🌐 Platform Advantages

- 🧠 **No bookmaker** bias — compete only against other individuals
- 🔒 **Non-custodial** — funds are locked in smart contracts
- 🌍 **Censorship-resistant** — win as much as you want, no bans
- 🧾 **Transparent rules** — all conditions are verifiable on-chain

---

## 🧠 Future Improvements

- Dynamic oracle integrations
- Multi-outcome bets (not just win/loss)
- Match history and player statistics feed
- Liquidity pool for automated market making

---

## 📄 License

MIT License — see [LICENSE](./LICENSE)

---

## 👤 Author

Developed by [Lorys Hamadache](https://github.com/LorysHamadache)  
As part of the [EMURGO CDP](https://education.emurgo.io/) Smart Contract Developer Program
