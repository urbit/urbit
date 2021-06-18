
export default function ResumeParse(settings) {
  let parser = {};
  function create() {
    parser.current = this.Parser;
    Object.assign(this.Parser, settings);
  }

  return [parser, create]
}
