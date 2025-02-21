import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Types "types";
import Nat64 "mo:base/Nat64";
import Time "mo:base/Time";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.
    
    //Types
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;

    //Project 1

    let name : Text = "ICP bootcamp";
    var manifesto : Text = "Learn more about web3 and motoko for the ICP bootcamp";
    var goals : Buffer.Buffer<Text> = Buffer.Buffer<Text>(10);

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    //Project 2

    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

       public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                members.put(caller, member);
                return #ok();
            };
            case(? oldMember){
                return #err("The principal is already linked to a member profile");
            }
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch(members.get(p)){
            case(null){
                return #err("No member linked to this principal");
            };
            case(? member){
                return #ok(member);
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #err("No member profile linked with your principal");
            };
            case(? oldMember){
                members.put(caller, member);
                return #ok();
            }
        };
    };

    public query func getAllMembers() : async [Member] {
        let iterator = members.vals();
        return Iter.toArray(iterator);
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #err("No member linked to your principal");
            };
            case(? oldMember){
                members.delete(caller);
                return #ok();
            }
        };
    };


    //Project 3

    let wallet = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

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


    //Project 4


    let proposals = HashMap.HashMap<ProposalId, Proposal>(0,Nat64.equal, Nat64.toNat32);
    stable var NewProposalId : Nat64 = 0;

    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        if(Option.isNull(members.get(caller))){
            return #err("A member is needed, you have to create one !");
        };

        let balanceCaller = Option.get(wallet.get(caller), 0);
        if(balanceCaller < 1){
            return #err("Your balance is not right, be careful !");
        };

        let newProposal = {
            id = NewProposalId;
            content;
            creator = caller;
            created = Time.now();
            executed = null;
            votes = [];
            voteScore = 0;
            status = #Open;
        };

        proposals.put(NewProposalId, newProposal);
        _burn(caller, 1);
        NewProposalId += 1;
        return #ok(NewProposalId - 1);
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        if(Option.isNull(members.get(caller))){
            return #err("A member is needed, you have to create one !");
        };
        switch(proposals.get(proposalId)){
            case(null){
                return #err("A proposal is needed !");
            };
            case(? proposal){
                if (_hasVoted(proposal, caller)){
                    return #err("1 vote per proposal, you can't vote more");
                };
                let newProposal = _newProposal(proposal, caller, yesOrNo);
                proposals.put(proposal.id ,newProposal);
                if(newProposal.status == #Accepted){
                    _execute(newProposal);
                };
                return #ok;
            };
        };
    };


    func _hasVoted(proposal: Proposal, p: Principal) : Bool {
        for(vote in proposal.votes.vals()){
            if(vote.member == p){
                return true;
            };
        };
        return false;
    };

    func _newProposal(proposal : Proposal, voter : Principal, yesOrNo : Bool) : Proposal{
        let votingPower = Option.get(wallet.get(voter), 0);
        let multiplier = switch(yesOrNo){
            case(true){1};
            case(false){-1};
        };

        let callerVoteScore = votingPower * multiplier;
        let newVotes = Buffer.fromArray<Vote>(proposal.votes);

        newVotes.add({
            member = voter;
            votingPower;
            yesOrNo;
        });

        let newScore = proposal.voteScore + callerVoteScore;
        
        let newStatus = if(newScore >= 100){
            #Accepted;
        } else if (newScore <= -100){
            #Rejected;
        }
        else{
            #Open;
        };

        let newProposal = {
            id = proposal.id;
            content = proposal.content;
            creator = proposal.creator;
            created = proposal.created;
            executed = proposal.executed;
            votes = Buffer.toArray(newVotes);
            voteScore = newScore;
            status = newStatus;
        };
        return newProposal;
    };


    func _execute(proposal: Proposal) : () {
        switch(proposal.content){
            case(#ChangeManifesto(newManifesto)){
                manifesto := newManifesto;
            };
            case(#AddGoal(newGoal)){
                goals.add(newGoal);
            };
        };
        let newProposal = {
            id = proposal.id;
            content = proposal.content;
            creator = proposal.creator;
            created = proposal.created;
            executed = ?Time.now();
            votes = proposal.votes;
            voteScore = proposal.voteScore;
            status = proposal.status;
            
            };
            proposals.put(proposal.id, newProposal);
            return;
    };
};