import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Option "mo:base/Option";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let wallet = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);
    let name : Text = "The token";
    let symbol : Text = "Sym";

    public query func tokenName() : async Text {
        return name;
    };

    public query func tokenSymbol() : async Text {
        return symbol;
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let ownerBalance = Option.get (wallet.get(owner), 0);
        wallet.put(owner, ownerBalance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let ownerBalance = Option.get(wallet.get(owner), 0);
        if(amount > ownerBalance){
            return #err("Unfortunately, not enough token to use");
        };
        wallet.put(owner, ownerBalance - amount);
        return #ok();
    };
    
    func _burn(owner: Principal, amount:Nat) : () {
        let balance = Option.get(wallet.get(owner), 0);
        wallet.put(owner, balance-amount);
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceOrigins = Option.get(wallet.get(from), 0);
        let balanceDestination = Option.get(wallet.get(to), 0);
        if(balanceOrigins < amount){
            return #err("Not enough credits from the Origins to do the transfer");
        };
        wallet.put(from, balanceOrigins - amount);
        wallet.put(to, balanceDestination + amount);
        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        return Option.get(wallet.get(account), 0);
    };

    public query func totalSupply() : async Nat {
        var balanceTotal = 0;
        for(balance in wallet.vals()){
            balanceTotal := balanceTotal + balance;
        };
        return balanceTotal;
    };
};