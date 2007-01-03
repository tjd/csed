/*
 * Created on July 30, 2004
 */
package misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

import csimage.util.EasyInput;
import csimage.util.dbg;



/**
 * @author I-Ling
 */
public class SimpleWebPageReader {

    public static void main(String[] args) {
        SimpleWebPageReader sb = new SimpleWebPageReader();
        String url = EasyInput
                .input("Please enter Web page address you want retrieve (enter to quit): ");
        while (url.length() > 0) {
            dbg.sayln("you type " + url);
            String pageContent = sb.getWebPage(url);
            dbg.sayln(pageContent);
            url = EasyInput
                    .input("Please enter Web page address you want retrieve (enter to quit): ");
        } // while
        System.out.println("Bye!");
    }

    //
    // implementing GetWebPage
    //
    public String getWebPage(String url) {
        String content = "";
        URL urlObj = null;
        try {
            urlObj = new URL(url);
        } catch (MalformedURLException urlEx) {
            urlEx.printStackTrace();
            throw new Error("URL creation failed.");
            //System.exit(1);
        }

        try {
            BufferedReader reader = new BufferedReader(
                                                       new InputStreamReader(
                                                                             urlObj
                                                                                     .openStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                content += line;
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new Error("Page retrieval failed.");
            //System.exit(1);
        }
        return content;
    }
}