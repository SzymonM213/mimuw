import com.squareup.moshi.Moshi;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Main {

    public static void main(String[] args) throws IOException {

        File wejscie = new File(args[0]);

        String dane = new String(Files.readAllBytes(Path.of(wejscie.getPath())));

        Moshi moshi = new Moshi.Builder().build();

        
    }
}
