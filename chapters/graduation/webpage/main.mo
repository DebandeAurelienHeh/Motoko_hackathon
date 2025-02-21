import Types "types";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Blob "mo:base/Blob";

actor Webpage {

    type Result<A, B> = Result.Result<A, B>;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    // ID du DAO canister (à remplacer par l'ID réel après déploiement)
    stable let daoCanisterId : Principal = Principal.fromText("aaaaa-aa");
    
    // Le manifeste doit toujours correspondre à celui du DAO
    stable var manifesto : Text = "Let's graduate!";

    // Affiche le manifeste actuel via HTTP
    public query func http_request(request : HttpRequest) : async HttpResponse {
        ({
            status_code = 200;
            headers = [("Content-Type", "text/plain; charset=utf-8")];
            body = Text.encodeUtf8(manifesto);
            streaming_strategy = null;
        });
    };

    // Seul le DAO peut modifier le manifeste
    public shared ({ caller }) func setManifesto(newManifesto : Text) : async Result<(), Text> {
        // Vérification de l'identité de l'appelant
        if (caller != daoCanisterId) {
            return #err("Unauthorized: Only DAO can update manifesto");
        };
        
        // Mise à jour du manifeste
        manifesto := newManifesto;
        #ok()
    };
};