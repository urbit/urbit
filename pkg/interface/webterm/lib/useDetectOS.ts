/* eslint-disable no-useless-escape */
// Regex patterns inspired by:
// https://github.com/faisalman/ua-parser-js/blob/master/src/ua-parser.js
const LINUX = [
  /\b(joli|palm)\b ?(?:os)?\/?([\w\.]*)/i,
  /(mint)[\/\(\) ]?(\w*)/i,
  /(mageia|vectorlinux)[; ]/i,
  /([kxln]?ubuntu|debian|suse|opensuse|gentoo|arch(?= linux)|slackware|fedora|mandriva|centos|pclinuxos|red ?hat|zenwalk|linpus|raspbian|plan 9|minix|risc os|contiki|deepin|manjaro|elementary os|sabayon|linspire)(?: gnu\/linux)?(?: enterprise)?(?:[- ]linux)?(?:-gnu)?[-\/ ]?(?!chrom|package)([-\w\.]*)/i,
  /(hurd|linux) ?([\w\.]*)/i,
  /(gnu) ?([\w\.]*)/i,
  /\b([-frentopcghs]{0,5}bsd|dragonfly)[\/ ]?(?!amd|[ix346]{1,2}86)([\w\.]*)/i,
  /(haiku) (\w+)/i,
  /(sunos) ?([\w\.\d]*)/i,
  /((?:open)?solaris)[-\/ ]?([\w\.]*)/i,
  /(aix) ((\d)(?=\.|\)| )[\w\.])*/i,
  /\b(beos|os\/2|amigaos|morphos|openvms|fuchsia|hp-ux)/i,
  /(unix) ?([\w\.]*)/i
];

const MAC_OS = [
  /(mac os x) ?([\w\. ]*)/i,
  /(macintosh|mac_powerpc\b)(?!.+haiku)/i
];

const WINDOWS = [
  /microsoft (windows) (vista|xp)/i,
  /(windows) nt 6\.2; (arm)/i,
  /(windows (?:phone(?: os)?|mobile))[\/ ]?([\d\.\w ]*)/i,
  /(windows)[\/ ]?([ntce\d\. ]+\w)(?!.+xbox)/i
];

export const useDetectOS = () => {
  const userAgent = navigator.userAgent;

  const isLinux = LINUX.some(regex => regex.test(userAgent));
  const isMacOS = MAC_OS.some(regex => regex.test(userAgent));
  const isWindows = WINDOWS.some(regex => regex.test(userAgent));

  return {
    isLinux,
    isMacOS,
    isWindows
  };
};
