import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;

describe("POI NFT Contract", () => {
    it("should allow contract owner to verify addresses", () => {
        const verifyTx = simnet.callPublicFn("POI", "verify-address", 
            [Cl.principal(wallet1)], 
            deployer
        );
        expect(verifyTx.result).toBeOk(Cl.bool(true));
    });

    it("should allow verified address to mint POI NFT", () => {
        // First verify the address
        simnet.callPublicFn("POI", "verify-address", 
            [Cl.principal(wallet1)], 
            deployer
        );

        // Then mint NFT
        const mintTx = simnet.callPublicFn("POI", "mint", 
            [], 
            wallet1
        );
        expect(mintTx.result).toBeOk(Cl.uint(1));
    });

    it("should not allow unverified address to mint", () => {
        const mintTx = simnet.callPublicFn("POI", "mint", 
            [], 
            wallet2
        );
        expect(mintTx.result).toBeErr(Cl.uint(101));
    });
});
