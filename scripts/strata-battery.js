// strata-battery.js — quality battery for the strata engine: long prose,
// code (writes /dev/shm/st/merge.py for execution-checking), OpenAI+Anthropic
// tool calling, multi-turn memory. Run: node scripts/strata-battery.js [base-url]
// strata quality battery: long prose, executed-code check, tool calling (OpenAI + Anthropic), multi-turn.
const BASE=process.argv[2]||"https://strata.ts.peteches.co.uk";
const https=require("https");
process.env.NODE_TLS_REJECT_UNAUTHORIZED="0";
function post(path,body,timeoutMs=900000){return new Promise((res,rej)=>{
  const data=JSON.stringify(body);const u=new URL(BASE+path);
  const req=https.request({hostname:u.hostname,path:u.pathname,method:"POST",
    headers:{"Content-Type":"application/json","Content-Length":Buffer.byteLength(data)}},r=>{
    let b="";r.on("data",c=>b+=c);r.on("end",()=>{try{res(JSON.parse(b))}catch(e){rej(new Error("badjson:"+b.slice(0,300)))}});});
  req.setTimeout(timeoutMs,()=>req.destroy(new Error("timeout")));req.on("error",rej);req.end(data);});}
const chat=(messages,opts={})=>post("/v1/chat/completions",Object.assign({model:"strata",messages,max_tokens:2048},opts));
const t0=()=>Date.now();
const results=[];
function verdict(name,pass,detail,ms){results.push({name,pass});
  console.log(`\n### ${pass?"PASS":"FAIL"} ${name} (${(ms/1000).toFixed(1)}s)\n${detail}`);}
const hasBangs=s=>/(!)\1{7,}/.test(s);
async function main(){
{const t=t0();const r=await chat([{role:"user",content:"Write an atmospheric scene of 400-600 words: a lighthouse keeper in a storm realizes the light has gone out. Prose only, no preamble."}],{max_tokens:4096});
 const c=r.choices[0].message.content||"";const words=c.split(/\s+/).length;
 verdict("prose-long",!hasBangs(c)&&words>=300&&r.choices[0].finish_reason==="stop",
  `words=${words} finish=${r.choices[0].finish_reason}\n${c.slice(0,350)}...\n${c.slice(-200)}`,Date.now()-t);}
{const t=t0();
 const r=await chat([{role:"user",content:"Write a Python function merge_intervals(intervals) that merges overlapping intervals. Return a new sorted list of merged [start,end] pairs. Handle: empty input, single interval, nested intervals, touching intervals ([1,3],[3,5] -> [1,5]). Reply with ONLY one ```python code block, no explanation."}],{max_tokens:3072,temperature:0.3});
 const c=r.choices[0].message.content||"";
 const m=c.match(/```python\n([\s\S]*?)```/);
 if(!m){verdict("code-exec",false,"no python block:\n"+c.slice(0,300),Date.now()-t);}
 else{require("fs").writeFileSync("/dev/shm/st/merge.py",m[1]+`
if __name__=="__main__":
    import json
    cases=[([],[]),([[1,3]],[[1,3]]),([[1,3],[2,6],[8,10],[15,18]],[[1,6],[8,10],[15,18]]),([[1,3],[3,5]],[[1,5]]),([[1,10],[2,3],[4,5]],[[1,10]]),([[5,6],[1,2]],[[1,2],[5,6]]),([[1,4],[0,4]],[[0,4]]),([[1,4],[2,3]],[[1,4]]),([[5,6],[6,7],[1,1]],[[1,1],[5,7]])]
    out=[]
    for inp,exp in cases:
        got=merge_intervals([list(x) for x in inp])
        out.append({"inp":inp,"exp":exp,"got":got,"ok":sorted(map(tuple,got))==sorted(map(tuple,exp))})
    print(json.dumps(out))
`);
 verdict("code-extracted",!hasBangs(c),`code ${m[1].length}ch saved for execution`,Date.now()-t);}}
{const t=t0();
 const tools=[{type:"function",function:{name:"get_weather",description:"Get current weather for a location",parameters:{type:"object",properties:{location:{type:"string"},unit:{type:"string",enum:["celsius","fahrenheit"]}},required:["location"]}}}];
 const r=await chat([{role:"user",content:"What is the weather in Edinburgh right now? Use the tool."}],{tools,tool_choice:"auto",max_tokens:2048});
 const msg=r.choices[0].message;const tc=(msg.tool_calls||[])[0];
 let pass1=!!tc&&tc.function&&tc.function.name==="get_weather"&&/edinburgh/i.test(tc.function.arguments||"");
 let detail=`tool_call=${tc?tc.function.name+"("+tc.function.arguments+")":"NONE"} finish=${r.choices[0].finish_reason}`;
 let pass2=false;
 if(pass1){
   const r2=await chat([{role:"user",content:"What is the weather in Edinburgh right now? Use the tool."},
     Object.assign({},msg,{content:msg.content||null}),
     {role:"tool",tool_call_id:tc.id,content:JSON.stringify({location:"Edinburgh",temperature_c:12,condition:"light rain",wind_kph:24})}],{tools,tool_choice:"auto",max_tokens:2048});
   const final=r2.choices[0].message.content||"";
   pass2=/12/.test(final)&&/rain/i.test(final)&&!hasBangs(final);
   detail+=`\nfinal: ${final.slice(0,200)}`;}
 verdict("tools-openai",pass1&&pass2,detail,Date.now()-t);}
{const t=Date.now();
 const body={model:"strata",max_tokens:2048,
   tools:[{name:"get_weather",description:"Get current weather",input_schema:{type:"object",properties:{location:{type:"string"}},required:["location"]}}],
   messages:[{role:"user",content:"Weather in Glasgow? Use the tool."}]};
 const r=await post("/v1/messages",body);
 const tu=(r.content||[]).find(b=>b.type==="tool_use");
 let pass=!!tu&&tu.name==="get_weather"&&/glasgow/i.test(JSON.stringify(tu.input||{}));
 let detail=`tool_use=${tu?tu.name+" "+JSON.stringify(tu.input):"NONE"} stop=${r.stop_reason}`;
 if(pass){
   body.messages.push({role:"assistant",content:r.content});
   body.messages.push({role:"user",content:[{type:"tool_result",tool_use_id:tu.id,content:JSON.stringify({location:"Glasgow",temperature_c:9,condition:"overcast"})}]});
   const r2=await post("/v1/messages",body);
   const txt=(r2.content||[]).filter(b=>b.type==="text").map(b=>b.text).join(" ");
   detail+=`\nfinal: ${txt.slice(0,200)}`;
   pass=pass&&/9|overcast/i.test(txt);}
 verdict("tools-anthropic",pass,detail,Date.now()-t);}
{const t=t0();
 const msgs=[{role:"user",content:"Remember two facts: my codeword is FERROUS-GLASS and my favourite number is 73. Reply OK only."}];
 const r1=await chat(msgs,{max_tokens:1024,temperature:0.2});
 msgs.push({role:"assistant",content:r1.choices[0].message.content||""});
 msgs.push({role:"user",content:"What is my codeword and my favourite number? One line."});
 const r2=await chat(msgs,{max_tokens:1024,temperature:0.2});
 const c2=r2.choices[0].message.content||"";
 verdict("multi-turn",/FERROUS-GLASS/.test(c2)&&/73/.test(c2),`reply: ${c2.slice(0,150)}`,Date.now()-t);}
const npass=results.filter(r=>r.pass).length;
console.log(`\n===== BATTERY SUMMARY: ${npass}/${results.length} passed =====`);
process.exit(npass===results.length?0:1);
}
main().catch(e=>{console.error("BATTERY-ERROR",e.message);process.exit(1)});
