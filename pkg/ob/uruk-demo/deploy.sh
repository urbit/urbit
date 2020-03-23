SERVER=ec2-3-92-173-246.compute-1.amazonaws.com
ROUTE=http://3.92.173.246
EMAIL=benjamin+urukdemo@tlon.io
KEY=~/r/personal/2020-03/uruk-demo.pem
ob deploy init           \
  --ssh-key "$KEY"       \
  --hostname "$SERVER"   \
  --route "$ROUTE"       \
  --admin-email "$EMAIL" \
  --disable-https        \
  ~/uruk-demo-deploy
