#!/bin/bash
version=$1
mkdir -p $version/ipop.src
mkdir $version/tmp
git clone ../../ipop $version/tmp/ipop_tmp
git clone ../../brunet $version/tmp/brunet_tmp
mkdir $version/ipop.src/ipop
mkdir $version/ipop.src/brunet
cp -axf $version/tmp/ipop_tmp/* $version/ipop.src/ipop/.
cp -axf $version/tmp/brunet_tmp/* $version/ipop.src/brunet/.

cd ..
nant
cd -

mkdir -p $version/ipop/bin
lib_files="Brunet.dll Brunet.Dht.dll Brunet.XmlRpc.dll Mono.Security.dll Brunet.Coordinate.dll Brunet.Security.dll NDesk.Options.dll CookComputing.XmlRpcV2.dll System.Runtime.Remoting.dll.use_for_mono_2"
brunet_bin_files="P2PNode.exe"
ipop_bin_files="DhtIpopNode.exe GroupVPNService.exe"
for file in $lib_files; do
  cp ../lib/$file $version/ipop/bin/.
done

for file in $brunet_bin_files; do 
  cp ../../brunet/bin/$file $version/ipop/bin/.
done

for file in $ipop_bin_files; do
  cp ../bin/$file $version/ipop/bin/.
done

for file in $ipop_lib_files; do
  cp ../lib/$file $version/ipop/bin/.
done

cp ../scripts/groupvpn* $version/ipop/bin/.
cp ../scripts/daemon.py $version/ipop/bin/.
cp ../scripts/install* $version/ipop/.

mkdir $version/ipop/deb
cp deb/* $version/ipop/deb/.

cp -axf ../drivers $version/ipop/.

brunet_scripts="bget.py bput.py crawl.py pybru.py"
for file in $brunet_scripts; do
  cp ../../brunet/scripts/$file $version/ipop/bin/.
done

cp -axf ../config $version/ipop/.
cp ../docs/release_notes.txt $version/ipop/.
cp ../README $version/ipop/.
echo $version > $version/ipop/version

cd $version
zip -r9 ipop.src.$version.zip ipop.src
zip -r9 ipop.$version.zip ipop
cd -
mv $version/ipop.src.$version.zip .
mv $version/ipop.$version.zip .
rm -rf $version
