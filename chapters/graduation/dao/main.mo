import Result "mo:base/Result";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Time "mo:base/Time";
import Types "types";

actor {

    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;

    // The principal of the Webpage canister associated with this DAO canister
    stable let canisterIdWebpage : Principal = Principal.fromText("xumeo-zyaaa-aaaab-qadaa-cai");
    stable var manifesto = "Graduate from motoko Bootcamp et succeed the ICP bootcamp";
    stable let name = "ICP bootcamp";
    stable var goals : [Text] = [];

    // Array to store members (stable)
    stable var members : [(Principal, Member)] = [];

    // Array to store proposals (stable)
    stable var proposals : [(ProposalId, Proposal)] = [];

    // Counter for generating unique proposal IDs
    stable var proposalIdCounter : Nat = 0;

    // Register a new member in the DAO with the given name and principal of the caller
    public shared ({ caller }) func registerMember(name : Text) : async Result<(), Text> {
        switch (Array.find<(Principal, Member)>(members, func ((p, _)) : Bool { p == caller })) {
            case (null) {
                let newMember : Member = {
                    name = name;
                    role = #Student;
                };
                members := Array.append<(Principal, Member)>(members, [(caller, newMember)]);
                return #ok();
            };
            case (? _) {
                return #err("The principal is already linked to a member profile");
            };
        };
    };

    // Get the member with the given principal
    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (Array.find<(Principal, Member)>(members, func ((principal, _)) : Bool { principal == p })) {
            case (null) {
                return #err("No member linked to this principal");
            };
            case (? (_, member : Member)) {
                return #ok(member);
            };
        };
    };

    // Graduate the student with the given principal
    public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
        switch (Array.find<(Principal, Member)>(members, func ((p, _)) : Bool { p == caller })) {
            case (null) {
                return #err("Caller is not a member");
            };
            case (? (_, callerMember : Member)) {
                if (callerMember.role != #Mentor) {
                    return #err("Caller is not a mentor");
                };
                switch (Array.find<(Principal, Member)>(members, func ((p, _)) : Bool { p == student })) {
                    case (null) {
                        return #err("Student does not exist");
                    };
                    case (? (_, studentMember : Member)) {
                        if (studentMember.role != #Student) {
                            return #err("Member is not a student");
                        };
                        let updatedMember : Member = {
                            name = studentMember.name;
                            role = #Graduate;
                        };
                        members := Array.map<(Principal, Member), (Principal, Member)>(members, func ((p, m)) : (Principal, Member) {
                            if (p == student) (p, updatedMember) else (p, m)
                        });
                        return #ok();
                    };
                };
            };
        };
    };

    // Create a new proposal and returns its id
    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch (Array.find<(Principal, Member)>(members, func ((p, _)) : Bool { p == caller })) {
            case (null) {
                return #err("Caller is not a member");
            };
            case (? (_, member : Member)) {
                if (member.role != #Mentor) {
                    return #err("Caller is not a mentor");
                };
                let proposalId = proposalIdCounter;
                proposalIdCounter += 1;
                let proposal : Proposal = {
                    id = proposalId;
                    content = content;
                    creator = caller;
                    created = Time.now();
                    executed = null;
                    votes = [];
                    voteScore = 0;
                    status = #Open;
                };
                proposals := Array.append<(ProposalId, Proposal)>(proposals, [(proposalId, proposal)]);
                return #ok(proposalId);
            };
        };
    };

    // Get the proposal with the given id
    public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
        switch (Array.find<(ProposalId, Proposal)>(proposals, func ((pId, _)) : Bool { pId == id })) {
            case (null) {
                return #err("Proposal does not exist");
            };
            case (? (_, proposal : Proposal)) {
                return #ok(proposal);
            };
        };
    };

    // Returns all the proposals
    public query func getAllProposal() : async [Proposal] {
        return Array.map<(ProposalId, Proposal), Proposal>(proposals, func ((_, p)) : Proposal { p });
    };

    // Vote for the given proposal
    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        switch (Array.find<(ProposalId, Proposal)>(proposals, func ((pId, _)) : Bool { pId == proposalId })) {
            case (null) {
                return #err("Proposal does not exist");
            };
            case (? (_, proposal : Proposal)) {
                if (proposal.status != #Open) {
                    return #err("Proposal is not open for voting");
                };
                switch (Array.find<(Principal, Member)>(members, func ((p, _)) : Bool { p == caller })) {
                    case (null) {
                        return #err("Caller is not a member");
                    };
                    case (? (_, member : Member)) {
                        if (Array.find<Vote>(proposal.votes, func (v : Vote) : Bool { v.member == caller }) != null) {
                            return #err("Caller has already voted");
                        };
                        let vote : Vote = {
                            member = caller;
                            votingPower = 1; // Assuming each member has a voting power of 1
                            yesOrNo = yesOrNo;
                        };
                        let updatedProposal : Proposal = {
                            proposal with 
                            votes = Array.append<Vote>(proposal.votes, [vote]);
                            voteScore = proposal.voteScore + (if (yesOrNo) 1 else -1);
                        };
                        proposals := Array.map<(ProposalId, Proposal), (ProposalId, Proposal)>(proposals, func ((pId, p)) : (ProposalId, Proposal) {
                            if (pId == proposalId) (pId, updatedProposal) else (pId, p)
                        });
                        return #ok();
                    };
                };
            };
        };
    };

    // Returns the Principal ID of the Webpage canister associated with this DAO canister
    public query func getIdWebpage() : async Principal {
        return canisterIdWebpage;
    };
};