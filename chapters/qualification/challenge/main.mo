actor MotivationLetter {
    let name : Text = "Debande Aurelien";
    var message : Text = "I want to learn more about web3 and Motoko for the ICP bootcamp";
    public func setMessage(newMessage : Text) : async (){
        message := newMessage;
        return;
    };
    public query func getMessage() : async Text {
        return message;
    };
    public query func getName() : async Text {
        return name;
    };
} 
    