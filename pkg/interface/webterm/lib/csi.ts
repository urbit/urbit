export const csi = (cmd: string, ...args: number[]) => {
  return '\x1b[' + args.join(';') + cmd;
};
