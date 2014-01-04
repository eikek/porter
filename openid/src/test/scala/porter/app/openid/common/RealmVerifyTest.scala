package porter.app.openid.common

import org.scalatest.WordSpec

class RealmVerifyTest extends WordSpec {

  import porter.app.openid.routes.Implicits._
  import spray.http.Uri

  "A realm verifiyer" must {

    "properly find mismatches" in {
      assert(Uri("http://example.com/path").matchesRealm("http://example.com/path#frag") === false)
      assert(Uri("https://example.com/").matchesRealm("http://example.com/") === false)
      assert(Uri("http://example.com/").matchesRealm("https://example.com/") === false)
      assert(Uri("http://example.com:82").matchesRealm("https://example.com/") === false)
      assert(Uri("http://example.com").matchesRealm("https://example.com:88") === false)
      assert(Uri("https://example.com/path/other").matchesRealm("https://example.com/Path") === false)
    }

    "match correct urls" in {
      assert(Uri("http://example.com/path").matchesRealm("http://example.com/path") === true)
      assert(Uri("http://example.com:122/path").matchesRealm("http://example.com:122/path") === true)
      assert(Uri("https://example.com:9443/path/other").matchesRealm("https://example.com:9443/path") === true)
    }
  }
}
