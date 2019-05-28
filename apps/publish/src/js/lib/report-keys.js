export const REPORT_KEYS = [
  'landscape.prize',
        // /circle/<cir_name>/grams
        // call automatically on inbox
        // call automatically on /urbit-meta
        // call automatically on any DM circles created
  'circle.gram',
  'circle.nes',
        // /circle/<cir_name>/config-l
        // used for loading inbox config
  'circle.cos.loc',
        // /circle/<cir_name>/config-r
        // used for loading inbox's sources' configs
  'circle.cos.rem',
        // /circle/<cir_name>/config-l
        // used for fora topic creation....maybe? let me check

  'circle.config',
  'circle.config.dif.full',
        // /circle/<cir_name>/config-l
        // used for subscription / unsubscription
  'circle.config.dif.source',
        // /circles, required for initialization
  'circles',

        // frontend specific, no server calls
  'menu.toggle',
  'config.ext',
  'inbox.sources-loaded',
  'circle.read',
  'dm.new',
  'dm.clear',
];


