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

    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
    stable let canisterIdWebpage : Principal = Principal.fromText("aaaaa-aa");
    stable var manifesto = "Graduate from motoko Bootcamp et succeed the ICP bootcamp";
    stable let name = "ICP bootcamp";
    stable var goals = Buffer.Buffer<Text>(0);

    // HashMap to store members
    stable var members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    // HashMap to store proposals
    stable var proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat64.equal, Nat64.hash);

    // Counter for generating unique proposal IDs
    stable var proposalIdCounter : Nat64 = 0;

    // Returns the name of the DAO
    public query func getName() : async Text {
        return name;
    };

    // Returns the manifesto of the DAO
    public query func getManifesto() : async Text {
        return manifesto;
    };

    // Returns the goals of the DAO
    public query func getGoals() : async [Text] {
        return Buffer.toArray(goals);
    };

    // Register a new member in the DAO with the given name and principal of the caller
    // Airdrop 10 MBC tokens to the new member
    // New members are always Student
    // Returns an error if the member already exists
    public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (? oldMember) {
                return #err("The principal is already linked to a member profile");
            };
        };
    };

    // Get the member with the given principal
    // Returns an error if the member does not exist
    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("No member linked to this principal");
            };
            case (? member) {
                return #ok(member);
            };
        };
    };

    // Graduate the student with the given principal
    // Returns an error if the student does not exist or is not a student
    // Returns an error if the caller is not a mentor
    public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Caller is not a member");
            };
            case (? callerMember) {
                if (callerMember.role != #Mentor) {
                    return #err("Caller is not a mentor");
                };
                switch (members.get(student)) {
                    case (null) {
                        return #err("Student does not exist");
                    };
                    case (? studentMember) {
                        if (studentMember.role != #Student) {
                            return #err("Member is not a student");
                        };
                        let updatedMember : Member = {
                            name = studentMember.name;
                            role = #Graduate;
                            tokens = studentMember.tokens;
                        };
                        members.put(student, updatedMember);
                        return #ok();
                    };
                };
            };
        };
    };

    // Create a new proposal and returns its id
    // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Caller is not a member");
            };
            case (? member) {
                if (member.role != #Mentor) {
                    return #err("Caller is not a mentor");
                };
                if (member.tokens < 1) {
                    return #err("Caller does not own at least 1 MBC token");
                };
                let proposalId = proposalIdCounter;
                proposalIdCounter += 1;
                let proposal : Proposal = {
                    id = proposalId;
                    content = content;
                    proposer = caller;
                    votesFor = 0;
                    votesAgainst = 0;
                    voters = [];
                    executed = false;
                };
                proposals.put(proposalId, proposal);
                return #ok(proposalId);
            };
        };
    };

    // Get the proposal with the given id
    // Returns an error if the proposal does not exist
    public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
        switch (proposals.get(id)) {
            case (null) {
                return #err("Proposal does not exist");
            };
            case (? proposal) {
                return #ok(proposal);
            };
        };
    };

    // Returns all the proposals
    public query func getAllProposal() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    // Vote for the given proposal
    // Returns an error if the proposal does not exist or the member is not allowed to vote
    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        switch (proposals.get(proposalId)) {
            case (null) {
                return #err("Proposal does not exist");
            };
            case (? proposal) {
                if (proposal.executed) {
                    return #err("Proposal has already been executed");
                };
                switch (members.get(caller)) {
                    case (null) {
                        return #err("Caller is not a member");
                    };
                    case (? member) {
                        if (member.tokens < 1) {
                            return #err("Caller does not own at least 1 MBC token");
                        };
                        if (Array.find(proposal.voters, func (v : Principal) : Bool { v == caller }) != null) {
                            return #err("Caller has already voted");
                        };
                        let updatedProposal : Proposal = if (yesOrNo) {
                            { proposal with votesFor = proposal.votesFor + 1; voters = Array.append(proposal.voters, [caller]) };
                        } else {
                            { proposal with votesAgainst = proposal.votesAgainst + 1; voters = Array.append(proposal.voters, [caller]) };
                        };
                        proposals.put(proposalId, updatedProposal);
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