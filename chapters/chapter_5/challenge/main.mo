import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Nat64 "mo:base/Nat64";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Types "types";

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
    type DAOStats = Types.DAOStats;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

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

    //Project 5

    let logo : Text =
            "<svg version='1.0' xmlns='http://www.w3.org/2000/svg'
            width='512.000000pt' height='512.000000pt' viewBox='0 0 512.000000 512.000000'
            preserveAspectRatio='xMidYMid meet'>
            <g transform='translate(0.000000,512.000000) scale(0.100000,-0.100000)'
            fill='#000000' stroke='none'>
            <path d='M2560 4900 c-450 -50 -870 -286 -1110 -640 -91 -132 -160 -269 -210
            -420 -49 -148 -65 -239 -76 -395 -31 -460 118 -884 434 -1232 98 -108 279
            -247 426 -328 105 -57 152 -79 319 -149 167 -70 191 -82 276 -132 51 -31 111
            -77 132 -102 l38 -46 -60 -27 c-185 -84 -400 -269 -526 -456 -58 -87 -134
            -225 -134 -239 0 -5 79 -10 176 -10 172 0 179 1 202 23 12 12 55 79 95 147
            124 210 272 391 425 510 72 56 73 58 57 78 -32 41 -111 101 -196 150 -47 28
            -86 53 -86 58 0 15 27 57 61 95 107 121 173 264 202 440 14 80 14 108 4 165
            -15 81 -39 148 -74 208 l-23 40 54 17 c238 74 519 280 661 487 209 307 279
            719 193 1112 -120 540 -592 970 -1172 1078 -133 26 -340 37 -476 22z m435
            -110 c488 -91 855 -437 968 -922 27 -119 25 -364 -4 -482 -91 -373 -364 -658
            -730 -750 -109 -27 -369 -24 -489 5 -344 86 -635 312 -785 610 -68 136 -93
            260 -93 439 0 181 26 300 98 445 185 370 587 570 1035 519z'/>

            <path d='M2470 4535 c-171 -27 -338 -110 -462 -232 -190 -190 -283 -443 -261
            -708 14 -164 50 -277 136 -424 61 -106 222 -267 331 -331 243 -141 519 -149
            775 -23 234 115 391 320 442 576 24 118 14 328 -20 432 -57 174 -165 321 -320
            433 -186 136 -396 191 -621 177z m288 -110 c220 -51 410 -218 493 -437 43
            -114 52 -186 45 -338 -5 -111 -10 -140 -35 -205 -36 -91 -100 -190 -166 -257
            -80 -81 -187 -142 -299 -172 -122 -33 -290 -22 -416 28 -215 85 -360 255
            -412 487 -19 84 -19 272 0 357 54 231 207 407 432 490 116 41 230 50 358 25z'/>

            <path d='M1435 1325 l-330 -330 330 -330 330 -330 330 330 330 330 -330 330
            -330 330 -330 -330z m495 -165 l170 -170 -172 -172 -173 -173 -172 172 -173
            173 170 170 c94 94 172 170 175 170 3 0 81 -76 175 -170z'/>
            <path d='M3025 1325 l-330 -330 330 -330 330 -330 330 330 330 330 -330 330
            -330 330 -330 -330z m495 -165 l170 -170 -172 -172 -173 -173 -172 172 -173
            173 170 170 c94 94 172 170 175 170 3 0 81 -76 175 -170z'/>
            </g>
            </svg>";
    
    func _getWebpage() : Text {
        var webpage = "<style>" #
        "body { text-align: center; font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; }" #
        "h1 { font-size: 3em; margin-bottom: 10px; }" #
        "hr { margin-top: 20px; margin-bottom: 20px; }" #
        "em { font-style: italic; display: block; margin-bottom: 20px; }" #
        "ul { list-style-type: none; padding: 0; }" #
        "li { margin: 10px 0; }" #
        "li:before { content: '👉 '; }" #
        "svg { max-width: 150px; height: auto; display: block; margin: 20px auto; }" #
        "h2 { text-decoration: underline; }" #
        "</style>";

        webpage := webpage # "<div><h1>" # name # "</h1></div>";
        webpage := webpage # "<em>" # manifesto # "</em>";
        webpage := webpage # "<div>" # logo # "</div>";
        webpage := webpage # "<hr>";
        webpage := webpage # "<h2>Our goals:</h2>";
        webpage := webpage # "<ul>";
        for (goal in goals.vals()) {
            webpage := webpage # "<li>" # goal # "</li>";
        };
        webpage := webpage # "</ul>";
        return webpage;
    };

    func _getMemberNames() : [Text] {
        let memberArray = Iter.toArray(members.vals());
        return Array.map<Member, Text>(memberArray, func(member :Member){member.name});
    };

    public query func getStats() : async DAOStats {
        return ({
            name = "ICP bootcamp";
            manifesto = "Learn more about web3 and motoko for the ICP bootcamp";
            goals = Buffer.toArray(goals);
            members = _getMemberNames();
            logo;
            numberOfMembers = members.size();
        });
    };

    public query func http_request(request : HttpRequest) : async HttpResponse {
        return ({
            status_code = 200;
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            body = Text.encodeUtf8(_getWebpage());
            streaming_strategy = null;
        });
    };
};