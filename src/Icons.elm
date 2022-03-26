module Icons exposing (..)

import Svg
import VirtualDom exposing (Attribute, attribute)


type alias Icon msg =
    List (Attribute msg) -> Svg.Svg msg


reset : List (Attribute msg) -> Svg.Svg msg
reset attrs =
    Svg.node "svg" ([ attribute "viewBox" "0 0 40 40", attribute "fill" "none", attribute "xmlns" "http://www.w3.org/2000/svg" ] ++ attrs) [ Svg.node "path" [ attribute "d" "M16.6667 18.3333H11.835L11.8367 18.3183C12.0543 17.2527 12.4803 16.2406 13.09 15.34C13.9975 14.003 15.2703 12.9552 16.7567 12.3216C17.26 12.1083 17.785 11.945 18.32 11.8366C19.4299 11.6116 20.5736 11.6116 21.6834 11.8366C23.2783 12.1635 24.7419 12.9523 25.8917 14.105L28.2517 11.7516C27.1881 10.6877 25.9296 9.83838 24.545 9.24998C23.8389 8.95103 23.1047 8.72323 22.3534 8.56998C20.8031 8.25499 19.2053 8.25499 17.655 8.56998C16.9031 8.72387 16.1684 8.95221 15.4617 9.25164C13.3793 10.1351 11.5966 11.6018 10.3284 13.475C9.47485 14.7381 8.87798 16.1566 8.57171 17.65C8.52504 17.875 8.50004 18.105 8.46671 18.3333H3.33337L10 25L16.6667 18.3333ZM23.3334 21.6666H28.165L28.1634 21.68C27.7279 23.8162 26.4687 25.6951 24.6584 26.91C23.7581 27.5203 22.7458 27.9462 21.68 28.1633C20.5708 28.3883 19.4276 28.3883 18.3184 28.1633C17.2528 27.9457 16.2406 27.5198 15.34 26.91C14.8977 26.6108 14.4848 26.2704 14.1067 25.8933L11.75 28.25C12.8142 29.3136 14.0732 30.1624 15.4584 30.75C16.165 31.05 16.9034 31.2783 17.65 31.43C19.1997 31.7451 20.797 31.7451 22.3467 31.43C25.3343 30.8099 27.9622 29.049 29.6717 26.5216C30.5244 25.2595 31.1207 23.8421 31.4267 22.35C31.4717 22.125 31.4984 21.895 31.5317 21.6666H36.6667L30 15L23.3334 21.6666Z" ] [] ]


sun : List (Attribute msg) -> Svg.Svg msg
sun attrs =
    Svg.node "svg" ([ attribute "viewBox" "0 0 40 40", attribute "fill" "none", attribute "xmlns" "http://www.w3.org/2000/svg" ] ++ attrs) [ Svg.node "path" [ attribute "fill-rule" "evenodd", attribute "clip-rule" "evenodd", attribute "d" "M20 26.6667C21.7681 26.6667 23.4638 25.9643 24.714 24.714C25.9643 23.4638 26.6667 21.7681 26.6667 20C26.6667 18.2319 25.9643 16.5362 24.714 15.286C23.4638 14.0357 21.7681 13.3333 20 13.3333C18.2319 13.3333 16.5362 14.0357 15.286 15.286C14.0357 16.5362 13.3333 18.2319 13.3333 20C13.3333 21.7681 14.0357 23.4638 15.286 24.714C16.5362 25.9643 18.2319 26.6667 20 26.6667ZM20 30C22.6522 30 25.1957 28.9464 27.0711 27.0711C28.9464 25.1957 30 22.6522 30 20C30 17.3478 28.9464 14.8043 27.0711 12.9289C25.1957 11.0536 22.6522 10 20 10C17.3478 10 14.8043 11.0536 12.9289 12.9289C11.0536 14.8043 10 17.3478 10 20C10 22.6522 11.0536 25.1957 12.9289 27.0711C14.8043 28.9464 17.3478 30 20 30ZM18.3333 0H21.6667V6.77C20.5598 6.63194 19.4402 6.63194 18.3333 6.77V0ZM11.8233 9.46667L7.03667 4.68L4.68 7.03667L9.46667 11.8233C10.1512 10.9429 10.9429 10.1512 11.8233 9.46667ZM6.77 18.3333H0V21.6667H6.77C6.63194 20.5598 6.63194 19.4402 6.77 18.3333ZM9.46667 28.1767L4.68 32.9633L7.03667 35.32L11.8233 30.5333C10.9429 29.8488 10.1512 29.0571 9.46667 28.1767ZM18.3333 33.23V40H21.6667V33.23C20.5599 33.3682 19.4401 33.3682 18.3333 33.23ZM28.1767 30.5333L32.9633 35.32L35.32 32.9633L30.5333 28.1767C29.8488 29.0571 29.0571 29.8488 28.1767 30.5333ZM33.23 21.6667H40V18.3333H33.23C33.3682 19.4401 33.3682 20.5599 33.23 21.6667ZM30.5333 11.8233L35.32 7.03667L32.9633 4.68L28.1767 9.46667C29.0567 10.15 29.8483 10.9433 30.5333 11.8233Z" ] [] ]


moon : List (Attribute msg) -> Svg.Svg msg
moon attrs =
    Svg.node "svg" ([ attribute "viewBox" "0 0 24 23", attribute "fill" "none", attribute "xmlns" "http://www.w3.org/2000/svg" ] ++ attrs) [ Svg.node "path" [ attribute "d" "M0.73999 11.4399C0.73999 9.91994 1.03999 8.45994 1.62999 7.06994C2.21999 5.67994 3.02999 4.48994 4.02999 3.47994C5.02999 2.46994 6.22999 1.66994 7.61999 1.07994C9.00999 0.489941 10.46 0.189941 11.99 0.189941C13.52 0.189941 14.97 0.489941 16.36 1.07994C17.75 1.66994 18.95 2.47994 19.96 3.47994C20.97 4.47994 21.77 5.67994 22.36 7.06994C22.95 8.45994 23.25 9.90994 23.25 11.4399C23.25 12.9699 22.95 14.4199 22.36 15.8099C21.77 17.1999 20.96 18.3999 19.96 19.4099C18.96 20.4199 17.76 21.2199 16.36 21.8099C14.96 22.3999 13.51 22.6999 11.99 22.6999C10.47 22.6999 9.00999 22.3999 7.61999 21.8099C6.22999 21.2199 5.03999 20.4099 4.02999 19.4099C3.01999 18.4099 2.21999 17.2099 1.62999 15.8099C1.03999 14.4099 0.73999 12.9699 0.73999 11.4399V11.4399ZM1.93999 11.4399C1.93999 12.8099 2.20999 14.1099 2.73999 15.3499C3.26999 16.5899 3.98999 17.6599 4.88999 18.5599C5.78999 19.4599 6.85999 20.1699 8.09999 20.7099C9.33999 21.2499 10.64 21.5099 12 21.5099C12.36 21.5099 12.76 21.4899 13.2 21.4399C14.13 20.8699 14.91 20.1999 15.55 19.4099C16.19 18.6199 16.67 17.7699 16.98 16.8499C17.29 15.9299 17.51 15.0499 17.63 14.1999C17.75 13.3499 17.81 12.4299 17.81 11.4499C17.81 10.1999 17.66 8.98994 17.35 7.80994C17.04 6.62994 16.51 5.46994 15.76 4.31994C15.01 3.16994 14.07 2.20994 12.95 1.42994C12.54 1.40994 12.22 1.39994 12 1.39994C10.64 1.39994 9.33999 1.66994 8.09999 2.19994C6.85999 2.72994 5.78999 3.43994 4.88999 4.33994C3.98999 5.23994 3.27999 6.30994 2.73999 7.54994C2.19999 8.78994 1.93999 10.0899 1.93999 11.4399V11.4399Z" ] [] ]
