package porter.app.openid.common

import javax.swing._
import java.awt.event._
import porter.util.{ObjectRegistry, Hash}
import scala.util.Try
import Harmonicon._
import java.awt._
import scala.io.Codec

object HarmoniconPlay extends App {
  type HashFun = String => Vector[Byte]

  object Hashs extends ObjectRegistry {
    type Elem = HashFun
    val none: HashFun = register(_.getBytes(Codec.UTF8.charSet).toVector)
    val md5bytes: HashFun = register(s => Hash.md5(none(s)))
    val md5string: HashFun = register(s => none(Hash.md5String(s)))
    val sha1bytes: HashFun = register(s => Hash.sha1(none(s)))
    val sha1string: HashFun = register(s => none(Hash.sha1String(s)))
    val sha256bytes: HashFun = register(s => Hash.sha256(none(s)))
    val sha256string: HashFun = register(s => none(Hash.sha256String(s)))
    val sha384bytes: HashFun = register(s => Hash.sha384(none(s)))
    val sha384string: HashFun = register(s => none(Hash.sha384String(s)))
    val sha512bytes: HashFun = register(s => Hash.sha512(none(s)))
    val sha512string: HashFun = register(s => none(Hash.sha512String(s)))
  }


  val frame = {
    val f = new JFrame("Test it")
    f.setSize(800, 500)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.getContentPane.setLayout(new BorderLayout())
    f
  }
  val controls = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5))
    frame.getContentPane.add(p, BorderLayout.NORTH)
    p
  }

  val nameField = new JTextField("eike@eknet.org")
  nameField.setPreferredSize(new Dimension(300, 25))
  nameField.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent) = {
      if (e.getButton == MouseEvent.BUTTON1) {
        nameField.setText("")
      }
    }
  })
  val sizeField = new JTextField("100")
  sizeField.setPreferredSize(new Dimension(80, 25))
  val hashCombo = {
    val ident = identity[String]_
    val cb = new JComboBox[HashFun](new DefaultComboBoxModel[HashFun]() {
      Hashs.all.foreach(addElement)
    })
    cb.setRenderer(new MyRenderer)
    cb
  }

  val button = new JButton("OK")
  controls.add(new JLabel("Name: "))
  controls.add(nameField)
  controls.add(new JLabel("Size: "))
  controls.add(sizeField)
  controls.add(new JLabel("Hash: "))
  controls.add(hashCombo)
  controls.add(button)

  val icons = {
    val p = new JPanel(new GridLayout(0, 6))
    frame.getContentPane.add(new JScrollPane(p), BorderLayout.CENTER)
    p
  }

  button.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) = {
      val size = Try(Integer.parseInt(sizeField.getText)).getOrElse(100)
      val hash = hashCombo.getSelectedItem.asInstanceOf[HashFun]
      val icon = new AvatarPanel(nameField.getText, size, hash)
      icons.add(icon, 0)
      frame.getContentPane.validate()
    }
  })

  frame.setVisible(true)

  class AvatarPanel(name: String, size: Int, f: HashFun) extends JPanel with Scrollable {
    self =>
    val avatarIn = f(name.trim.toLowerCase)
    val avatar = Harmonicon.fromBytes(avatarIn, Timespan(), ImageSettings(size, Color.DARK_GRAY))
    setPreferredSize(new Dimension(size, size))
    setSize(getPreferredSize)
    setToolTipText(name)
    def getPreferredScrollableViewportSize = getPreferredSize
    def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int) = size
    def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int) = size
    def getScrollableTracksViewportWidth = true
    def getScrollableTracksViewportHeight = false

    addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent) = {
        if (e.getButton == MouseEvent.BUTTON3) {
          val parent = self.getParent
          parent.remove(self)
          parent.validate()
          parent.repaint()
        }
      }
    })
    override def paintComponent(g: Graphics) = {
      super.paintComponent(g)
      val x = (getWidth - size) / 2
      val y = (getHeight - size) / 2
      g.drawImage(avatar, x, y, null)
      g.setColor(Color.GRAY)
      g.drawRect(x, y, size -1, size -1)
    }
  }

  class MyRenderer extends ListCellRenderer[HashFun] {
    private val renderer:ListCellRenderer[AnyRef] = new DefaultListCellRenderer()

    def getListCellRendererComponent(list: JList[_ <: HashFun], value: HashFun, index: Int, isSelected: Boolean, cellHasFocus: Boolean) = {
      val display = value match {
        case Hashs.md5bytes => "MD5 (Bytes)"
        case Hashs.md5string => "MD5 (String)"
        case Hashs.sha1bytes => "SHA-1 (Bytes)"
        case Hashs.sha1string => "SHA-1 (String)"
        case Hashs.sha256bytes => "SHA-256 (Bytes)"
        case Hashs.sha256string => "SHA-256 (String)"
        case Hashs.sha384bytes => "SHA-384 (Bytes)"
        case Hashs.sha384string => "SHA-384 (String)"
        case Hashs.sha512bytes => "SHA-512 (Bytes)"
        case Hashs.sha512string => "SHA-512 (String)"
        case _ => "None"
      }
      renderer.getListCellRendererComponent(list, display, index, isSelected, cellHasFocus)
    }

  }
}
